{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}

module Bot.Conduit
    ( botC
    )
where

import           Conduit                   (ConduitT, (.|))
import qualified Conduit
import qualified Control.Concurrent        as Concurrent
import qualified Control.Concurrent.STM    as STM
import           Control.Monad             (forM_, forever, unless, when)
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Control.Monad.Logger      (MonadLogger, logDebugN, logErrorSH,
                                            logInfoN)
import           Control.Monad.Trans.Class (lift)
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import qualified Text.Megaparsec           as Megaparsec

import           Bot.Monad                 (MonadBot)
import qualified Bot.Monad                 as Bot
import           Irc.Event                 (Event (..))
import           Irc.EventHandler          (EventHandler (..))
import qualified Irc.Parser
import qualified Lifted

handlerC :: (MonadBot m, MonadLogger m)
    => [EventHandler m]
    -> ConduitT Text Text m ()
handlerC handlers = Conduit.awaitForever $ \msg ->
    case Megaparsec.parse Irc.Parser.eventP "" msg of
        Left  err   -> $(logErrorSH) err
        Right event -> do
            let matchingHandlers = filter
                    (\handler -> getEvent handler == event)
                    handlers
            forM_ matchingHandlers $ \handler ->
                lift (handlerFunc handler msg) >>= Conduit.yieldMany

newlineRemover :: Monad m => ConduitT Text Text m ()
newlineRemover = Conduit.awaitForever $ \msg ->
    Conduit.yield (Text.dropEnd 2 msg)

newlineAdder :: Monad m => ConduitT Text Text m ()
newlineAdder = Conduit.awaitForever $ \msg ->
    Conduit.yield (msg <> "\r\n")

serverLogger :: MonadLogger m => ConduitT Text Text m ()
serverLogger = Conduit.awaitForever $ \msg -> do
    let msg' = Text.replace "\n" "\n--> " msg
    mapM_ logInfoN $ Text.lines ("--> " <> msg')
    Conduit.yield msg

clientLogger :: MonadLogger m => ConduitT Text Text m ()
clientLogger = Conduit.awaitForever $ \msg -> do
    unless (Text.take 4 msg == "PASS") $
        logInfoN ("<-- " <> msg)
    Conduit.yield msg

onConnectC :: Monad m => m [Text] -> ConduitT Text Text m ()
onConnectC action = do
    lift action >>= Conduit.yieldMany
    Conduit.awaitForever Conduit.yield

rateLimiter :: (MonadLogger m, MonadIO m) => ConduitT Text Text m ()
rateLimiter = do
    -- Keeps track of how many messages have been sent in the last 30 secs
    msgsSent <- liftIO $ STM.newTVarIO (0 :: Int)

    -- Resets the number of messages sent to 0 every 30 seconds
    liftIO $ Concurrent.forkIO $ forever $ do
        Concurrent.threadDelay (30 * 1000000)
        STM.atomically (STM.writeTVar msgsSent 0)

    Conduit.awaitForever $ \msg -> do
        numMsgsSent <- Lifted.readTVarIOL msgsSent
        if numMsgsSent < 20
        then do
            Lifted.atomicallyL (STM.modifyTVar' msgsSent (+ 1))
            Conduit.yield msg
        else do
            logDebugN "Reached rate limit. Waiting..." 
            Lifted.atomicallyL $ do
                msgs <- STM.readTVar msgsSent
                unless (msgs == 0) STM.retry
            logDebugN "Rate limit reset."
            Conduit.yield msg

botC :: (MonadBot m, MonadLogger m, MonadIO m)
    => [EventHandler m]
    -> m [Text]
    -> ConduitT Text Text m ()
botC handlers onConnect =
    newlineRemover
        .| serverLogger
        .| handlerC handlers
        .| onConnectC onConnect
        .| rateLimiter
        .| clientLogger
        .| newlineAdder
