module Lifted where

import Control.Concurrent.STM
import Control.Monad.IO.Class (MonadIO, liftIO)

-- atomically, but generalised to MonadIO
atomicallyL :: MonadIO io => STM a -> io a
atomicallyL = liftIO . atomically

-- readTVarIO, but generalised to MonadIO
readTVarIOL :: MonadIO io => TVar a -> io a
readTVarIOL = liftIO . readTVarIO