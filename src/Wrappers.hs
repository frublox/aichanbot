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

addToMsgQueue :: UnicodeMessage -> Bot ()
addToMsgQueue msg = do
    s <- state
    atomicallyL $ writeTChan (s^.msgQueue) msg

send :: UnicodeMessage -> Bot ()
send msg = do
    s <- state

    numMsgsSent <- readTVarIOL (s^.msgsSent)

    case numMsgsSent < 20 of
        True -> do
            Irc.send msg
            atomicallyL $ modifyTVar' (s^.msgsSent) (+1)
        False -> addToMsgQueue msg

sendMsgQueue :: Bot ()
sendMsgQueue = do
    s <- state
    msg <- atomicallyL $ tryReadTChan (s^.msgQueue)

    case msg of
        Nothing -> return ()
        Just msg' -> send msg' >> sendMsgQueue

rateLimitTimer :: Bot ()
rateLimitTimer = do
    s <- state

    threadDelay 1000
    atomicallyL $ modifyTVar' (s^.timeLeft) (subtract 1)

    remaining <- readTVarIOL (s^.timeLeft)

    case remaining > 0 of
        False -> do
            atomicallyL $ do
                writeTVar (s^.timeLeft) 20
                writeTVar (s^.msgsSent) 0

            sendMsgQueue
            rateLimitTimer
        True -> rateLimitTimer

privMsg :: Text -> Text -> Bot ()
privMsg chan msgText = send $ RawMsg ("PRIVMSG " <> chan <> " :" <> msgText)

announce :: Text -> Bot ()
announce msg = do
    s <- state
    privMsg (s^.config.channel) msg

replyTo :: Maybe Text -> Text -> Bot ()
replyTo maybeUser msg = do
    s <- state
    privMsg (s^.config.channel) msg'

    where
        msg' = case maybeUser of
            Just user -> "@" <> user <> " " <> msg
            Nothing -> msg