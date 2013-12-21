{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Control.Applicative
import           Control.Concurrent
import           Control.Lens.TH
import           Data.ByteString (ByteString)
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
        handleFormSubmit = loginUser "username" "password" Nothing (\_ -> return ()) (redirect "/")

-- Triggers on the /signout page
handleSignout :: Handler App (AuthManager App) ()
handleSignout = logout >> redirect "/"


handleEventNew :: Handler App (AuthManager App) ()
handleEventNew = method GET handleForm <|> method POST handleFormSubmit
    where
        handleForm = render "event/new"
        handleFormSubmit = redirect "/"
