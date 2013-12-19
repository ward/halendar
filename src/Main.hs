{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Control.Applicative
import           Control.Lens.TH
import           Data.ByteString (ByteString)
import           Snap
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Session
import           Snap.Snaplet.Session.Backends.CookieSession

data App = App
    { _heist :: Snaplet (Heist App)
    , _sess :: Snaplet SessionManager
    , _auth :: Snaplet (AuthManager App)
    }

makeLenses ''App

instance HasHeist App where heistLens = subSnaplet heist

routes :: [(ByteString, Handler App App ())]
routes = [
    ("/signup", with auth handleNewUser),
    --("/signin", writeText "Signin page"),
    --("/navigate", writeText "Navigates the calendar"),
    --("/publish", writeText "Publish an event"),
    --("/delete", writeText "Delete an event"),
    ("", serveDirectory "static")
    ]

appInit :: SnapletInit App App
appInit = makeSnaplet "halendar" "The Haskell Calendar" Nothing $ do
    h <- nestSnaplet "heist" heist $ heistInit "templates"
    s <- nestSnaplet "sess" sess $ initCookieSessionManager "site_key.txt" "sess" (Just 3600)
    a <- nestSnaplet "auth" auth $ initJsonFileAuthManager defAuthSettings sess "users.json"
    addRoutes routes
    addAuthSplices h auth
    wrapSite (<|> heistServe)
    return $ App h s a

main :: IO ()
main = serveSnaplet defaultConfig appInit

-- Triggers on the /signup page
handleNewUser :: Handler App (AuthManager App) ()
handleNewUser = method GET handleForm <|> method POST handleFormSubmit
  where
    handleForm = render "signup"
    handleFormSubmit = registerUser "username" "password" >> redirect "/"
