{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Db (
    User(..)
  , Event(..)
  , createTables
  , saveEvent
  , getEvent
  , getEventsForUser
  , getEventsForRange
  , deleteEvent) where

import           Control.Applicative
import           Control.Monad
import           Data.List
import qualified Data.Text as T
import           Data.Time (UTCTime)
import qualified Database.SQLite.Simple as S
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.ToField
import           Database.SQLite.Simple.Internal
import           Database.SQLite.Simple.Ok
import           Text.Read
import           Snap.Snaplet
import           Snap.Snaplet.SqliteSimple

import           Application
import           Time

--------------------------------------------------------------------------------
-- Types
--
-- User
--   This type which will be used to simplify Auth's AuthUser type for usage in
--   our code.
-- Event
--   Used to represent an event. We won't create these manually, but instead
--   only create it when pulling one from the database. This ensures existence
--   of a user id.
-- This is done in this file since they are so interwoven with the database.
--------------------------------------------------------------------------------

-- Expect to only really use user ID and login name
data User = User Int T.Text deriving (Show)

-- Used in Event
data Repeat = Never | Daily | Monthly | Yearly
    deriving (Show, Read, Enum)

-- Need these to also be able to take it from/put it in the database
-- Note the graceful failure if the value in the database is not an integer
-- If it is an integer but doesn't fit in our Enum, then hissyfits will be thrown
instance FromField Repeat where
    fromField (Field (S.SQLInteger i) _) = Ok (toEnum . fromIntegral $ i :: Repeat)
    fromField _ = Ok Never
instance ToField Repeat where
    toField = S.SQLInteger . fromIntegral . fromEnum


data Event = Event
    { eventId :: Int
    , eventTitle :: T.Text
    , eventDescription :: T.Text
    , eventStart :: UTCTime
    , eventEnd :: UTCTime
    , eventRepeat :: Repeat
    , eventOwner :: Int
    } deriving (Show)
-- For pulling from the database
instance FromRow Event where
    fromRow = Event <$> field <*> field <*> field <*> field <*> field <*> field <*> field

-- Produces the repeated events based on how often it should repeat
-- Invalid jumps are skipped, for example 31 january, monthly rep makes next
-- event be in march
-- Uses the UTCTime functions from Time.hs
repeatEvent :: Event -> [Event]
repeatEvent e = map timesToEvent $ getRepeater (eventStart e, eventEnd e)
    where
        timesToEvent :: (UTCTime, UTCTime) -> Event
        timesToEvent (start, end) = e { eventStart = start, eventEnd = end }
        getRepeater :: (UTCTime, UTCTime) -> [(UTCTime, UTCTime)]
        getRepeater = case eventRepeat e of
            Yearly -> repeatYearly
            Monthly -> repeatMonthly
            Daily -> repeatDaily
            Never -> \(es, ee) -> [(es, ee)]

-- Used to sort events by time when getting a range of events.
eventTimeOrder :: Event -> Event -> Ordering
eventTimeOrder e1 e2 = case eventStart e1 `compare` eventStart e2 of
    EQ -> eventEnd e1 `compare` eventEnd e2
    LT -> LT
    GT -> GT


--------------------------------------------------------------------------------
-- Database creation
--
-- createTables is run at every start up. If it doesn't find the  eventstable,
-- it creates it. (Note that Auth handles the user table)
--------------------------------------------------------------------------------

tableExists :: S.Connection -> String -> IO Bool
tableExists conn tblName = do
    r <- S.query conn "SELECT name FROM sqlite_master WHERE type='table' AND name=?" (Only tblName)
    case r of
        [Only (_ :: String)] -> return True
        _ -> return False

createTables :: S.Connection -> IO ()
createTables conn = do
    -- Create `events` table if it doesn't exist yet
    isEventTableCreated <- tableExists conn "events"
    unless isEventTableCreated $
        S.execute_ conn
            (S.Query $
             T.concat [ "CREATE TABLE events ("
                      , "id INTEGER PRIMARY KEY,"
                      , "title TEXT NOT NULL,"
                      , "description TEXT,"
                      , "start TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,"
                      , "end TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,"
                      , "repeat INTEGER DEFAULT 0,"
                      , "user_id INTEGER,"
                      , "deleted INTEGER DEFAULT 0,"
                      -- Take note: if auth's spec changes, this needs changing too
                      , "FOREIGN KEY(user_id) REFERENCES users(uid))"
                      ])

--------------------------------------------------------------------------------
-- Communication with the database
--
-- Functions to get events from the database as well as saving to it.
--------------------------------------------------------------------------------

getEvent :: Maybe Int -> Handler App Sqlite [Event]
getEvent Nothing = return []
getEvent (Just eid) =
    query "SELECT id, title, description, start, end, repeat, user_id FROM events WHERE deleted = 0 AND id = ?" (Only eid)

getEventsForUser :: User -> Handler App Sqlite [Event]
getEventsForUser (User user_id _) =
    query "SELECT id, title, description, start, end, repeat, user_id FROM events WHERE deleted = 0 AND user_id = ?" (Only user_id)

-- Event is in range if its end is after the start of the range AND its start
-- is before the end of the range. This gets all those partially in the range.
getEventsForRange :: UTCTime -> UTCTime -> Handler App Sqlite [Event]
getEventsForRange start end = do
    nr <- nonRepeating
    r <- repeating
    return $ sortBy eventTimeOrder (nr ++ r)
    where
        nonRepeating :: Handler App Sqlite [Event]
        nonRepeating = query "SELECT id, title, description, start, end, repeat, user_id FROM events WHERE deleted = 0 AND repeat = 0 AND start < ? AND end > ?" (end, start)
        repeating :: Handler App Sqlite [Event]
        repeating = do
            re <- query "SELECT id, title, description, start, end, repeat, user_id FROM events WHERE deleted = 0 AND repeat != 0 AND start < ?" (Only end)
            return $ concatMap (takeWhile ((< end) . eventStart) . dropWhile ((< start) . eventEnd) . repeatEvent) re

saveEvent :: User -> Maybe [T.Text] -> Handler App Sqlite ()
saveEvent (User uid _) parameters = saveEvent' (parseEventParameters parameters)
    where
        saveEvent' Nothing = return ()
        saveEvent' (Just (title, description, start, end, repeats)) =
            execute "INSERT INTO events (title, description, start, end, repeat, user_id) VALUES (?,?,?,?,?,?)"
                      ( title
                      , description
                      , start
                      , end
                      , repeats
                      , uid
                      )

parseEventParameters :: Maybe [T.Text]
                     -> Maybe (T.Text, T.Text, UTCTime, UTCTime, Int)
parseEventParameters Nothing = Nothing
parseEventParameters (Just [title, description, start, end, repeats]) = do
    start'   <- readMaybe (T.unpack start)   :: Maybe UTCTime
    end'     <- readMaybe (T.unpack end)     :: Maybe UTCTime
    repeats' <- readMaybe (T.unpack repeats) :: Maybe Int
    return (title, description, start', end', repeats')
parseEventParameters _ = Nothing

deleteEvent :: User -> Event -> Handler App Sqlite ()
deleteEvent (User uid _) event =
    execute "UPDATE events SET deleted = 1 WHERE id = ? AND user_id = ?" (eventId event,uid)

