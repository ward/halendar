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
import qualified Data.Text as T
import           Data.Time (UTCTime)
import qualified Database.SQLite.Simple as S
import           Text.Read
import           Snap.Snaplet
import           Snap.Snaplet.SqliteSimple

import           Application
import           Time

-- AuthUser is turned into this type.
-- From this it also follows that we expect to only ever need the UID and the
-- login name of a user in our program.
-- If this changes: refactoring fun times!
-- User ID login
data User = User Int T.Text deriving (Show)

data Event = Event
    { eventId :: Int
    , eventTitle :: T.Text
    , eventDescription :: T.Text
    , eventStart :: UTCTime
    , eventEnd :: UTCTime
    -- If we make something deriving from Enum, would it easily go in the database?
    -- Could use fromEnum/toEnum?
    , eventRepeat :: Int
    , eventOwner :: Int
    } deriving (Show)

-- TODO: Can't this be written more concisely?
instance FromRow Event where
    fromRow = Event <$> field <*> field <*> field <*> field <*> field <*> field <*> field

-- Produces the repeated events based on how often it should repeat
-- Invalid jumps are skipped, for example 31 january, monthly rep makes next
-- event be in march
repeatEvent :: Event -> [Event]
repeatEvent e = map timesToEvent $ getRepeater (eventStart e, eventEnd e)
    where
        timesToEvent :: (UTCTime, UTCTime) -> Event
        timesToEvent (start, end) = e { eventStart = start, eventEnd = end }
        getRepeater :: (UTCTime, UTCTime) -> [(UTCTime, UTCTime)]
        getRepeater = case (eventRepeat e) of
            3 -> repeatYearly
            2 -> repeatMonthly
            1 -> repeatDaily
            _ -> \(es, ee) -> [(es, ee)]


--------------------------------------------------------------------------------
-- Database creation

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
getEvent :: Maybe Int -> Handler App Sqlite [Event]
getEvent Nothing = return []
getEvent (Just eid) =
    query "SELECT id, title, description, start, end, repeat, user_id FROM events WHERE deleted = 0 AND id = ?" (Only eid)

getEventsForUser :: User -> Handler App Sqlite [Event]
getEventsForUser (User user_id _) =
    query "SELECT id, title, description, start, end, repeat, user_id FROM events WHERE deleted = 0 AND user_id = ?" (Only user_id)

getEventsForRange :: UTCTime -> UTCTime -> Handler App Sqlite [Event]
getEventsForRange start end = do
    nr <- nonRepeating
    r <- repeating
    return $ nr ++ r
    where
        nonRepeating :: Handler App Sqlite [Event]
        nonRepeating = query "SELECT id, title, description, start, end, repeat, user_id FROM events WHERE deleted = 0 AND repeat = 0 AND ((start BETWEEN ? AND ?) OR (end BETWEEN ? AND ?))" (start, end, start, end)
        repeating :: Handler App Sqlite [Event]
        repeating = do
            re <- query "SELECT id, title, description, start, end, repeat, user_id FROM events WHERE deleted = 0 AND repeat != 0 AND start < ?" (Only end)
            return re
            -- TODO:
            -- For each event:
            -- Repeat daily/monthly/yearly -> inf list
            -- drop while eventend is smaller than start
            -- take while start is smaller than end
            -- create events with the relevant start and end dates
            -- 
            -- flatten everything
            --return $ concatMap () re

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

