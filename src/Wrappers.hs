{-# LANGUAGE OverloadedStrings #-}

module Wrappers where

import Data.Text (Text)
import Data.Monoid ((<>))

import Control.Lens
import Control.Concurrent.Lifted (threadDelay)
import Control.Concurrent.STM
import Control.Monad.Reader
import Control.Monad.State hiding (state)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Network.IRC.Client hiding (send)
import qualified Network.IRC.Client as Irc

import Types
import Lifted (readTVarIOL, atomicallyL)

-- Run a computation in the StatefulIRC monad from the Bot monad
runStatefulIRC :: StatefulIRC IrcEnv a -> Bot a
runStatefulIRC action = do
    ircS <- ask
    liftIO (runReaderT action ircS)

addToMsgQueue :: UnicodeMessage -> Bot ()
addToMsgQueue msg = do
    msgQueueRef <- use (botState.msgQueue)
    atomicallyL (writeTChan msgQueueRef msg)

send :: UnicodeMessage -> Bot ()
send msg = do
    msgsSentRef <- use (botState.msgsSent)
    numMsgsSent <- readTVarIOL msgsSentRef

    case numMsgsSent < 20 of
        True -> do
            runStatefulIRC (Irc.send msg)
            atomicallyL (modifyTVar' msgsSentRef (+1))
        False -> addToMsgQueue msg

sendMsgQueue :: Bot ()
sendMsgQueue = do
    msgQueueRef <- use (botState.msgQueue)
    msg <- atomicallyL (tryReadTChan msgQueueRef)

    case msg of
        Nothing -> return ()
        Just msg' -> send msg' >> sendMsgQueue

rateLimitTimer :: Bot ()
rateLimitTimer = do
    timeLeftRef <- use (botState.timeLeft)

    threadDelay 1000

    atomicallyL $ modifyTVar' timeLeftRef (subtract 1)

    remaining <- readTVarIOL timeLeftRef

    case remaining > 0 of
        False -> do
            msgsSentRef <- use (botState.msgsSent)

            atomicallyL $ do
                writeTVar timeLeftRef 20
                writeTVar msgsSentRef 0

            sendMsgQueue
            rateLimitTimer
        True -> rateLimitTimer

privMsg :: Text -> Text -> Bot ()
privMsg chan msgText = send $ RawMsg ("PRIVMSG " <> chan <> " :" <> msgText)

announce :: Text -> Bot ()
announce msg = do
    chan <- use (config.channel)
    privMsg chan msg

replyTo :: Maybe Text -> Text -> Bot ()
replyTo maybeUser msg = do
    chan <- use (config.channel)
    privMsg chan msg'

    where
        msg' = case maybeUser of
            Just user -> "@" <> user <> " " <> msg
            Nothing -> msg