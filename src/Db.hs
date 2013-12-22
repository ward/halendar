{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Db (
    User(..)
  , Event(..)
  , createTables
  , saveEvent
  , getEventsForUser) where

import           Control.Applicative
import           Control.Monad
import qualified Data.Text as T
import           Data.Time (UTCTime)
import qualified Database.SQLite.Simple as S
import           Text.Read
import           Snap.Snaplet
import           Snap.Snaplet.SqliteSimple

import           Application

-- AuthUser is turned into this type.
-- From this it also follows that we expect to only ever need the UID and the
-- login name of a user in our program.
-- If this changes: refactoring fun times!
-- User ID login
data User = User Int T.Text

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
                      -- Take note: if auth's spec changes, this needs changing to
                      , "FOREIGN KEY(user_id) REFERENCES users(uid))"
                      ])

getEventsForUser :: User -> Handler App Sqlite [Event]
getEventsForUser (User user_id _) =
    query "SELECT id, title, description, start, end, repeat, user_id FROM events WHERE user_id = ?" (Only user_id)

saveEvent :: User -> Maybe [T.Text] -> Handler App Sqlite ()
saveEvent (User uid _) parameters = saveEvent' uid (parseEventParameters parameters)
    where
        saveEvent' uid Nothing = return ()
        saveEvent' uid (Just (title, description, start, end, repeats)) =
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
