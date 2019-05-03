module Irc
    ( run
    ) where

import           Conduit
import           Control.Lens
import           Data.ByteString      (ByteString)
import           Data.Conduit.Network
import           Data.Text            (Text)

run :: (Monad m, MonadIO m, MonadThrow m)
    => Int
    -> ByteString
    -> ConduitT Text Text m ()
    -> (m () -> IO ())
    -> IO ()
run port host appC runM = runTCPClient (clientSettings port host) app
    where
        app ad = runM . runConduit $
            appSource ad
            .| decodeUtf8C
            .| appC
            .| encodeUtf8C
            .| appSink ad
