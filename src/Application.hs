module Application where

data App = App
    { _heist :: Snaplet (Heist App)
    , _sess :: Snaplet SessionManager
    , _auth :: Snaplet (AuthManager App)
    , _db :: Snaplet Sqlite
    }

makeLenses ''App

instance HasHeist App where heistLens = subSnaplet heist
