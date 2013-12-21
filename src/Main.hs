{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Control.Applicative
import           Control.Concurrent
import           Control.Lens.TH
import           Data.ByteString (ByteString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Snap
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.SqliteSimple
import           Snap.Snaplet.Session
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Snaplet.SqliteSimple
----
import           Application
import           Db


routes :: [(ByteString, Handler App App ())]
routes = [
    ("/signup", with auth handleNewUser),
    ("/signin", with auth handleSignin),
    ("/signout", with auth handleSignout),
    ("/event/new", with auth handleEventNew),
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
        handleFormSubmit _ = do
            -- Vars become Maybe ByteString
            --title       <- getParam "title"
            --description <- getParam "description"
            --start       <- getParam "start"
            --end         <- getParam "end"
            ---- Prelude already has repeat
            --repeats     <- getParam "repeat"
            -- withTop prevents type mismatch since saveEvent returns
            -- Handler App Sqlite ()
            -- parameters is now [Maybe ByteString]
            parameters <- mapM getParam ["title", "description", "start", "end", "repeat"]
            -- sequence parameters is Maybe [ByteString]
            -- decode into Maybe [T.Text]
            withTop db $ saveEvent (sequence parameters >>= (\a -> Just (map T.decodeUtf8 a)))
            redirect "/"

