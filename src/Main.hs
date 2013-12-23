{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Control.Applicative
import           Control.Concurrent
--import           Control.Lens.TH
import           Data.ByteString (ByteString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time
import           Text.Read
import           Snap
--import           Snap.Core
import           Snap.Util.FileServe
--import           Snap.Http.Server
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.SqliteSimple
--import           Snap.Snaplet.Session
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Snaplet.SqliteSimple
--import           Heist
import           Heist.SpliceAPI
import qualified Heist.Interpreted as I
----
import           Application
import           Db
import           Time


routes :: [(ByteString, Handler App App ())]
routes = [
    ("/signup", with auth handleNewUser),
    ("/signin", with auth handleSignin),
    ("/signout", with auth handleSignout),
    ("/event/new", with auth handleEventNew),
    ("/event/view/:eventid", with auth handleEventView),
    ("/event/delete/:eventid", with auth handleEventDelete),
    ("/calendar", with auth handleCalendar),
    ("/calendar/:year", with auth handleCalendar),
    ("/calendar/:year/:month", with auth handleCalendar),
    ("/calendar/:year/:month/:day", with auth handleCalendar),
    ("", serveDirectory "static")
    ]

appInit :: SnapletInit App App
appInit = makeSnaplet "halendar" "The Haskell Calendar" Nothing $ do
    h <- nestSnaplet "heist" heist $ heistInit "templates"
    s <- nestSnaplet "sess" sess $ initCookieSessionManager "site_key.txt" "sess" (Just 3600)

    -- Initialize auth that's backed by an sqlite database
    d <- nestSnaplet "db" db sqliteInit
    a <- nestSnaplet "auth" auth $ initSqliteAuth sess d

    -- Grab the DB connection pool from the sqlite snaplet and call
    -- into the Model to create all the DB tables if necessary.
    let c = sqliteConn $ d ^# snapletValue
    liftIO $ withMVar c $ \conn -> Db.createTables conn

    addRoutes routes
    -- Add <ifLoggedIn> and <ifLoggedOut> tags in the heist templates
    addAuthSplices h auth
    wrapSite (<|> heistServe)
    return $ App h s a d

main :: IO ()
main = serveSnaplet defaultConfig appInit

-- Triggers on the /signup page
handleNewUser :: Handler App (AuthManager App) ()
handleNewUser = method GET handleForm <|> method POST handleFormSubmit
    where
        handleForm = render "signup"
        handleFormSubmit = registerUser "username" "password" >> redirect "/"

-- Triggers on the /signin page
handleSignin :: Handler App (AuthManager App) ()
handleSignin = method GET handleForm <|> method POST handleFormSubmit
    where
        handleForm = render "signin"
        handleFormSubmit = loginUser "username" "password" Nothing (\_ -> redirect "/signin") (redirect "/")

-- Triggers on the /signout page
handleSignout :: Handler App (AuthManager App) ()
handleSignout = logout >> redirect "/"

-- This function can be used for every page that requires you to be logged in.
-- If not logged in, you are redirect to the signin page
--    (Downside: your target page is forgotten)
-- If logged in, the current user is turned into the User type defined in Db.hs
-- then this is passed into the function `action'
-- Based on the example in snaplet-sqlite-simple
withLoggedInUser :: (Db.User -> Handler App (AuthManager App) ()) -> Handler App (AuthManager App) ()
withLoggedInUser action =
    currentUser >>= go
    where
        go :: Maybe AuthUser -> Handler App (AuthManager App) ()
        go Nothing = redirect "/signin"
        go (Just u) = case userId u of
                           Just uid -> action (Db.User (read . T.unpack $ unUid uid) (userLogin u))
                           Nothing  -> redirect "/signin"

handleEventNew :: Handler App (AuthManager App) ()
handleEventNew = method GET (withLoggedInUser handleForm) <|> method POST (withLoggedInUser handleFormSubmit)
    where
        handleForm :: Db.User -> Handler App (AuthManager App) ()
        handleForm _ = render "event/new"
        handleFormSubmit :: Db.User -> Handler App (AuthManager App) ()
        handleFormSubmit user = do
            -- parameters is now [Maybe ByteString]
            parameters <- mapM getParam ["title", "description", "start", "end", "repeat"]
            -- sequence parameters is Maybe [ByteString]
            -- decode into Maybe [T.Text]
            -- withTop prevents type mismatch since saveEvent returns
            -- Handler App Sqlite ()
            withTop db $ saveEvent user (sequence parameters >>= (Just . map T.decodeUtf8))
            redirect "/"

handleEventView :: Handler App (AuthManager App) ()
handleEventView = method GET (withLoggedInUser handleShowEvent)
    where
        handleShowEvent :: Db.User -> Handler App (AuthManager App) ()
        handleShowEvent _ = do
            eventid <- getParam "eventid"
            event <- withTop db (findEvent eventid)
            case event of
                [] -> redirect "/"
                [e] -> renderWithSplices "event/view" $ splices e
                _ -> redirect "/"
        findEvent :: Maybe ByteString -> Handler App Sqlite [Event]
        findEvent Nothing = return []
        findEvent (Just eid) = getEvent (readMaybe (T.unpack (T.decodeUtf8 eid)))
        splices event = do
            "eventid" ## I.textSplice . T.pack . show $ eventId event
            "eventtitle" ## I.textSplice (eventTitle event)
            "eventdescription" ## I.textSplice (eventDescription event)
            "eventstart" ## I.textSplice . T.pack . show $ eventStart event
            "eventend" ## I.textSplice . T.pack . show $ eventEnd event
            "eventrepeat" ## I.textSplice . T.pack . show $ eventRepeat event
            "eventowner" ## I.textSplice . T.pack . show $ eventOwner event

handleEventDelete :: Handler App (AuthManager App) ()
handleEventDelete = method POST (withLoggedInUser handleDeleteEvent)
    where
        handleDeleteEvent :: Db.User -> Handler App (AuthManager App) ()
        handleDeleteEvent user = do
            eventid <- getParam "eventid"
            event <- withTop db $ findEvent eventid
            case event of
                [e] -> withTop db (deleteEvent user e) >> render "event/delete"
                _ -> redirect "/"
        findEvent :: Maybe ByteString -> Handler App Sqlite [Event]
        findEvent Nothing = return []
        findEvent (Just eid) = getEvent (readMaybe (T.unpack (T.decodeUtf8 eid)))

handleCalendar :: Handler App (AuthManager App) ()
handleCalendar = method GET (withLoggedInUser go)
    where
        go :: Db.User -> Handler App (AuthManager App) ()
        go _ = do
            year <- getParam "year"
            month <- getParam "month"
            day <- getParam "day"
            dispatch (readBSMaybe year) (readBSMaybe month) (readBSMaybe day)
        dispatch :: Maybe Int -> Maybe Int -> Maybe Int -> Handler App (AuthManager App) ()
        dispatch Nothing     _            _          = handleCalendarRedirect
        dispatch (Just year) Nothing      _          = handleCalendarYear year
        dispatch (Just year) (Just month) _          = handleCalendarMonth year month
        dispatch (Just year) (Just month) (Just day) = handleCalendarDay year month day

handleCalendarRedirect :: Handler App (AuthManager App) ()
handleCalendarRedirect = do
    now <- liftIO getCurrentTime
    redirect $ concat ["/calendar/", show (getYear now), "/", show (getMonth now)]

handleCalendarYear year = redirect "/"
handleCalendarMonth year month = redirect "/"
handleCalendarDay year month day = redirect "/"

-- Helper function. getParam returns Maybe ByteString and we always want to
-- convert it to something
readBSMaybe :: Read a => Maybe ByteString -> Maybe a
readBSMaybe Nothing = Nothing
readBSMaybe (Just bs) = readMaybe (T.unpack (T.decodeUtf8 bs))
