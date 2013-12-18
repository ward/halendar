{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Control.Applicative
import           Control.Lens.TH
import           Snap
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server
import           Snap.Snaplet.Heist

data App = App
    { _heist :: Snaplet (Heist App)
    }

makeLenses ''App

instance HasHeist App where heistLens = subSnaplet heist

appInit :: SnapletInit App App
appInit = makeSnaplet "halendar" "The Haskell Calendar" Nothing $ do
    hs <- nestSnaplet "heist" heist $ heistInit "templates"
    addRoutes [
        ("/signup", writeText "Signup page"),
        ("/signin", writeText "Signin page"),
        ("/navigate", writeText "Navigates the calendar"),
        ("/publish", writeText "Publish an event"),
        ("/delete", writeText "Delete an event"),
        ("", serveDirectory "static")
        ]
    wrapSite (<|> heistServe)
    return $ App hs

main :: IO ()
main = serveSnaplet defaultConfig appInit

--main :: IO ()
--main = quickHttpServe site

site :: Snap ()
site =
    ifTop (writeBS "hello world") <|>
    route [ ("foo", writeBS "bar")
          , ("echo/:echoparam", echoHandler)
          ] <|>
    dir "static" (serveDirectory ".")

echoHandler :: Snap ()
echoHandler = do
    param <- getParam "echoparam"
    maybe (writeBS "must specify echo/param in URL")
          writeBS param
