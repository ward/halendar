{-# LANGUAGE TemplateHaskell #-}

module Application where

import Control.Lens
import Snap
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth
import Snap.Snaplet.Session
import Snap.Snaplet.SqliteSimple

data App = App
    { _heist :: Snaplet (Heist App)
    , _sess :: Snaplet SessionManager
    , _auth :: Snaplet (AuthManager App)
    , _db :: Snaplet Sqlite
    }

makeLenses ''App

instance HasHeist App where heistLens = subSnaplet heist
