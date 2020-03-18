{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}

module Bot.Conduit
    ( handler
    , newlineStripper
    , newlineAdder
    , serverLogger
    , clientLogger
    , onConnectC
    , rateLimiter
    , botC
    )
where

import           Conduit
import           Control.Concurrent             ( forkIO
                                                , threadDelay
                                                )
import           Control.Concurrent.STM
import           Control.Monad                  ( forM_
                                                , forever
                                                , unless
                                                , when
                                                )
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Logger
import           Control.Monad.Trans.Class      ( lift )
import qualified Data.ByteString.Char8         as BytesC8
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Text.Encoding             ( decodeUtf8
                                                , encodeUtf8
                                                )
import           System.Exit                    ( die )
import           Text.Megaparsec

import           Bot.Monad                      ( MonadBot )
import qualified Bot.Monad                     as Bot
import           Irc.Event                      ( Event(..) )
import           Irc.EventHandler               ( EventHandler(..) )
import           Irc.Parser                     ( eventP )
import           Lifted                         ( atomicallyL
                                                , readTVarIOL
                                                )

handler
    :: (MonadBot m, MonadLogger m)
    => [EventHandler m]
    -> ConduitT Text Text m ()
handler handlers = awaitForever $ \msg -> do
    case parse eventP "" msg of
        Left  err   -> $(logErrorSH) err
        Right event -> runHandlers msg event

    lift Bot.checkMsgs >>= yieldMany
  where
    runHandlers msg event = forM_ handlers (handleEvent event msg)
    handleEvent event msg (EventHandler e f) = when (e == event) $ lift (f msg)

newlineStripper :: Monad m => ConduitT Text Text m ()
newlineStripper = awaitForever $ \msg -> yield (Text.dropEnd 2 msg)

newlineAdder :: Monad m => ConduitT Text Text m ()
newlineAdder = awaitForever $ \msg -> yield (msg <> "\r\n")

serverLogger :: MonadLogger m => ConduitT Text Text m ()
serverLogger = awaitForever $ \msg -> do
    let msg' = Text.replace "\n" "\n--> " msg
    mapM_ logInfoN $ Text.lines ("--> " <> msg')
    yield msg

clientLogger :: MonadLogger m => ConduitT Text Text m ()
clientLogger = awaitForever $ \msg -> do
    logInfoN ("<-- " <> msg)
    yield msg

onConnectC :: MonadBot m => m () -> ConduitT Text Text m ()
onConnectC action = do
    lift action
    lift Bot.checkMsgs >>= yieldMany
    awaitForever yield

rateLimiter :: MonadIO m => ConduitT Text Text m ()
rateLimiter = do
    -- Keep track of how many messages have been sent in the last 30 secs
    msgsSent <- liftIO (newTVarIO (0 :: Int))

    -- Reset the number of messages sent to 0 every 30 seconds
    let timer = forever $ do
            threadDelay (30 * 1000000)
            atomically (writeTVar msgsSent 0)

    liftIO (forkIO timer)

    let checkAndYield msg = do
            numMsgsSent <- readTVarIOL msgsSent
            if numMsgsSent < 20
                then do
                    atomicallyL (modifyTVar' msgsSent (+ 1))
                    yield msg
                else do
                    atomicallyL $ do
                        msgs <- readTVar msgsSent
                        unless (msgs == 0) retry
                    yield msg

    awaitForever checkAndYield

botC
    :: (MonadIO m, MonadBot m, MonadLogger m)
    => [EventHandler m]
    -> (forall m . MonadBot m => m ())
    -> ConduitT Text Text m ()
botC handlers onConnect =
    newlineStripper
        .| serverLogger
        .| handler handlers
        .| onConnectC onConnect
        .| rateLimiter
        .| clientLogger
        .| newlineAdder
