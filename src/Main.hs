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

