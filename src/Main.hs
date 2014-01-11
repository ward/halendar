{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Control.Applicative
import           Control.Concurrent
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time
import           Text.Read
import           Snap
import           Snap.Util.FileServe
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.SqliteSimple
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Snaplet.SqliteSimple
import           Heist.SpliceAPI
import qualified Heist.Interpreted as I
----
import           Application
import           Db
import           Time

--------------------------------------------------------------------------------
-- Snappy things
--
-- This part does all the setting up of Snap. This ranges from defining the
-- routes mapping to initializing the Snaplets.
-- The main function also resides here.
--------------------------------------------------------------------------------

-- Maps URLs to what we want to use to handle it.
routes :: [(BS.ByteString, Handler App App ())]
routes = [
    ("/signup",                     with auth handleNewUser),
    ("/signin",                     with auth handleSignin),
    ("/signout",                    with auth handleSignout),
    ("/event/new",                  with auth handleEventNew),
    ("/event/view/:eventid",        with auth handleEventView),
    ("/event/delete/:eventid",      with auth handleEventDelete),
    ("/calendar",                   with auth handleCalendar),
    ("/calendar/:year",             with auth handleCalendar),
    ("/calendar/:year/:month",      with auth handleCalendar),
    ("/calendar/:year/:month/:day", with auth handleCalendar),
    ("",                            serveDirectory "static")
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

--------------------------------------------------------------------------------
-- Some helpers
--
-- Before going to the functions actually handling the requests, here are some
-- helper functions.
--------------------------------------------------------------------------------

-- This function can be used for every page that requires you to be logged in.
-- You essentially wrap that function with this one.
-- If not logged in, you are shown an error
-- If logged in, the current user is turned into the User type defined in Db.hs
-- then this is passed into the function `action'
-- (Based on the example in snaplet-sqlite-simple)
withLoggedInUser :: (Db.User -> Handler App (AuthManager App) ()) -> Handler App (AuthManager App) ()
withLoggedInUser action =
    currentUser >>= go
    where
        go :: Maybe AuthUser -> Handler App (AuthManager App) ()
        go Nothing = renderError' "You need to be logged in to access this page."
        go (Just u) = case userId u of
                           Just uid -> action (Db.User (read . T.unpack $ unUid uid) (userLogin u))
                           Nothing  -> renderError' "You don't have a userid, what the hell?"

-- Used to output an error to the user where needed.
renderError :: Show a => a -> Handler App (AuthManager App) ()
renderError a = renderError' . show $ a
renderError' :: String -> Handler App (AuthManager App) ()
renderError' s = renderWithSplices "_error" $ do
    "errormsg" ## I.textSplice . T.pack $ s

-- Turn an event into splices. Needed for rendering.
renderEvent :: Monad n => Event -> Splices (I.Splice n)
renderEvent event = do
    "eventid"          ## I.textSplice . T.pack . show $ eventId event
    "eventtitle"       ## I.textSplice $ eventTitle event
    "eventdescription" ## I.textSplice $ eventDescription event
    "eventstart"       ## I.textSplice . T.pack . show $ eventStart event
    "eventend"         ## I.textSplice . T.pack . show $ eventEnd event
    "eventrepeat"      ## I.textSplice . T.pack . show $ eventRepeat event
    "eventowner"       ## I.textSplice . T.pack . show $ eventOwner event

renderEvents :: [Event] -> SnapletISplice App
renderEvents = I.mapSplices $ I.runChildrenWith . renderEvent

-- getParam returns Maybe BS.ByteString and we always want to convert it
readBSMaybe :: Read a => Maybe BS.ByteString -> Maybe a
readBSMaybe mbs = mbs >>= readMaybe . T.unpack . T.decodeUtf8

-- We do this pretty often too
toTextSplice :: Show a => a -> SnapletISplice App
toTextSplice = I.textSplice . T.pack . show

--------------------------------------------------------------------------------
-- User pages
--
-- These handle signing up, in and out. Note that they make use of the Auth
-- snaplet's functions.
--------------------------------------------------------------------------------    

-- Triggers on the /signup page
-- Form is shown on GET requests, form is handled on POST requests.
-- This idea is used on every page with a form.
handleNewUser :: Handler App (AuthManager App) ()
handleNewUser = method GET handleForm <|> method POST handleFormSubmit
    where
        handleForm = render "signup"
        handleFormSubmit = registerUser "username" "password" >>= afterRegister
        afterRegister :: Either AuthFailure AuthUser -> Handler App (AuthManager App) ()
        afterRegister (Left af) = renderError af
        afterRegister _ = render "_signupsuccess"

-- Triggers on the /signin page
handleSignin :: Handler App (AuthManager App) ()
handleSignin = method GET handleForm <|> method POST handleFormSubmit
    where
        handleForm = render "signin"
        handleFormSubmit = loginUser "username" "password" Nothing renderError (redirect "/")

-- Triggers on the /signout page
handleSignout :: Handler App (AuthManager App) ()
handleSignout = logout >> redirect "/"

--------------------------------------------------------------------------------
-- Event pages
--
-- Handlers for creating an event, viewing it and deleting it. The functions
-- defined in Db.hs are used to communicate with the database backend.
-- All of these require a logged in user.
--------------------------------------------------------------------------------

handleEventNew :: Handler App (AuthManager App) ()
handleEventNew = method GET (withLoggedInUser handleForm) <|> method POST (withLoggedInUser handleFormSubmit)
    where
        handleForm :: Db.User -> Handler App (AuthManager App) ()
        handleForm _ = render "event/new"
        handleFormSubmit :: Db.User -> Handler App (AuthManager App) ()
        handleFormSubmit user = do
            -- parameters is now [Maybe BS.ByteString]
            parameters <- mapM getParam ["title", "description", "start", "end", "repeat"]
            -- sequence parameters is Maybe [BS.ByteString]
            -- decode into Maybe [T.Text]
            withTop db $ saveEvent user (sequence parameters >>= (Just . map T.decodeUtf8))
            redirect "/"

handleEventView :: Handler App (AuthManager App) ()
handleEventView = method GET (withLoggedInUser handleShowEvent)
    where
        handleShowEvent :: Db.User -> Handler App (AuthManager App) ()
        handleShowEvent _ = do
            eventid <- getParam "eventid"
            event <- withTop db $ getEvent . readBSMaybe $ eventid
            case event of
                [] -> renderError' "Event not found."
                [e] -> renderWithSplices "event/view" $ renderEvent e
                _ -> renderError' "I don't know how you got here, but please report this error."

-- Deletion only triggers on POST requests to prevent someone linking to the
-- delete page and tricking unsuspecting users into deleting their event.
handleEventDelete :: Handler App (AuthManager App) ()
handleEventDelete = method POST (withLoggedInUser handleDeleteEvent)
    where
        handleDeleteEvent :: Db.User -> Handler App (AuthManager App) ()
        handleDeleteEvent user@(User uid _) = do
            eventid <- getParam "eventid"
            event <- withTop db $ getEvent . readBSMaybe eventid
            case event of
                [e] -> case eventOwner e == uid of
                    True -> withTop db (deleteEvent user e) >> render "event/delete"
                    False -> renderError' "You do not own this event and cannot delete it."
                _ -> renderError' "Event not found."


--------------------------------------------------------------------------------
-- Calendar pages
--
-- Handlers to show day/month/year range of events that the user requested.
-- Requires user to be logged in.
--------------------------------------------------------------------------------

-- Dispatches things to appropriate functions depending on how many parameters
-- were passed along (just year OR year and month OR year, month and day).
handleCalendar :: Handler App (AuthManager App) ()
handleCalendar = method GET (withLoggedInUser go)
    where
        go :: Db.User -> Handler App (AuthManager App) ()
        go _ = do
            year <- getParam "year"
            month <- getParam "month"
            day <- getParam "day"
            dispatch (readBSMaybe year) (readBSMaybe month) (readBSMaybe day)
        dispatch :: Maybe Integer -> Maybe Int -> Maybe Int -> Handler App (AuthManager App) ()
        dispatch Nothing     _            _          = handleCalendarRedirect
        dispatch (Just year) Nothing      _          = handleCalendarYear year
        dispatch (Just year) (Just month) Nothing    = handleCalendarMonth year month
        dispatch (Just year) (Just month) (Just day) = handleCalendarDay year month day

-- Redirect to current month view
handleCalendarRedirect :: Handler App (AuthManager App) ()
handleCalendarRedirect = do
    now <- liftIO getCurrentTime
    redirect . BSC.pack $ concat ["/calendar/", show (getYear now), "/", show (getMonth now)]

-- TODO: Next 3 are a bit too similar to my liking
-- Note that the similarity in the actual looks of the views is on purpose due
-- to time constraints
handleCalendarYear :: Integer -> Handler App (AuthManager App) ()
handleCalendarYear year = do
    let start = UTCTime (fromGregorian year 1 1) 0
    let end = UTCTime (fromGregorian year 12 31) 86401
    events <- withTop db $ getEventsForRange start end
    renderWithSplices "calendar/year" $ do
        "events"   ## renderEvents events
        "year"     ## toTextSplice year
        "prevyear" ## toTextSplice (year - 1)
        "nextyear" ## toTextSplice (year + 1)

handleCalendarMonth :: Integer -> Int -> Handler App (AuthManager App) ()
handleCalendarMonth year month = do
    let start = UTCTime (fromGregorian year month 1) 0
    let end = UTCTime (fromGregorian year month (gregorianMonthLength year month)) 86401
    let (prevyear, prevmonth, _) = toGregorian . addGregorianMonthsClip (-1) $ fromGregorian year month 1
    let (nextyear, nextmonth, _) = toGregorian . addGregorianMonthsClip 1 $ fromGregorian year month 1
    events <- withTop db $ getEventsForRange start end
    renderWithSplices "calendar/month" $ do
        "events"    ## renderEvents events
        "year"      ## toTextSplice year
        "month"     ## toTextSplice month
        "prevyear"  ## toTextSplice prevyear
        "prevmonth" ## toTextSplice prevmonth
        "nextyear"  ## toTextSplice nextyear
        "nextmonth" ## toTextSplice nextmonth

handleCalendarDay :: Integer -> Int -> Int -> Handler App (AuthManager App) ()
handleCalendarDay year month day = do
    let start = UTCTime (fromGregorian year month day) 0
    let end = UTCTime (fromGregorian year month day) 86401
    let (prevyear, prevmonth, prevday) = toGregorian . addDays (-1) $ fromGregorian year month day
    let (nextyear, nextmonth, nextday) = toGregorian . addDays 1 $ fromGregorian year month day
    events <- withTop db $ getEventsForRange start end
    renderWithSplices "calendar/day" $ do
        "events"    ## renderEvents events
        "year"      ## toTextSplice year
        "month"     ## toTextSplice month
        "day"       ## toTextSplice day
        "prevyear"  ## toTextSplice prevyear
        "prevmonth" ## toTextSplice prevmonth
        "prevday"   ## toTextSplice prevday
        "nextyear"  ## toTextSplice nextyear
        "nextmonth" ## toTextSplice nextmonth
        "nextday"   ## toTextSplice nextday
