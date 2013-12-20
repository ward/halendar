module Db (
    User(..)
  , Event(..)
  , createTables
  , saveEvent
  , getEventsForUser) where

import qualified Data.Text as T
import           Data.Time (UTCTime)
import qualified Database.SQLite.Simple as S
import           Snap.Snaplet
import           Snap.Snaplet.SqliteSimple

-- TODO: How does this relate to the Auth user?
-- Seems this is used as a simplified User, the example fills this up with only
-- the ID and the login name (in withLoginUser)
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

-- TODO: Need instance of fromRow most likely?

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
                      , "user_id INTEGER)" -- TODO: add SQLite foreign key constraint, need to know user table name for that
                      ])
    -- Given its a relation of user 1:many events, we can just put foreign key
    -- in the events database, no?
    -- Same idea for the `event_user` table
    --isEventUserTableCreated <- tableExists conn "event_user"
    --unless isEventUserTableCreated $
    --    S.execute_ conn
    --        (S.Query $
    --         T.concat [ "CREATE TABLE event_user ("
    --                  , 
    --                  ])

getEventsForUser :: User -> Handler App Sqlite [Event]
getEventsForUser (User user_id _) =
    query "SELECT id, title, description, start, end, repeat, user_id FROM events WHERE user_id = ?" (Only user_id)

saveEvent :: Event -> Handler App Sqlite ()
saveEvent e = execute "INSERT INTO events (id, title, description, start, end, repeat, user_id) VALUES (?,?,?,?,?,?,?)"
                      ( eventId e
                      , eventTitle e
                      , eventDescription e
                      , eventStart e
                      , eventEnd e
                      , eventRepeat e
                      , eventOwner e
                      )
