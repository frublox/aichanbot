{-# LANGUAGE OverloadedStrings #-}

module Wrappers where

import Data.Text (Text)
import Data.Monoid ((<>))

import Control.Lens
import Control.Concurrent.Lifted (threadDelay)
import Control.Concurrent.STM
import Control.Monad.IO.Class (MonadIO, liftIO)

import Network.IRC.Client hiding (send)
import qualified Network.IRC.Client as Irc

import Types
import Lifted

addToQueue :: UnicodeMessage -> StatefulIRC BotState ()
addToQueue msg = do
    s <- state
    atomicallyL $ writeTChan (s^.msgQueue) msg

send :: UnicodeMessage -> StatefulIRC BotState ()
send msg = do
    s <- state
    msgsSent' <- readTVarIOL (s^.msgsSent)

    case (msgsSent' < 20) of
        True -> do
            Irc.send msg
            atomicallyL $ modifyTVar' (s^.msgsSent) (+1)
        False -> addToQueue msg

sendQueue :: StatefulIRC BotState ()
sendQueue = do
    s <- state
    msg <- atomicallyL $ tryReadTChan (s^.msgQueue)

    case msg of
        Nothing -> return ()
        Just msg' -> send msg'

rateLimitTimer :: StatefulIRC BotState ()
rateLimitTimer = do
    s <- state
    threadDelay 1000
    atomicallyL $ modifyTVar' (s^.timeLeft) (subtract 1)

    remaining <- readTVarIOL (s^.timeLeft)

    case (remaining >= 0) of
        False -> do
            atomicallyL $ writeTVar (s^.timeLeft) 20
            atomicallyL $ writeTVar (s^.msgsSent) 0

            sendQueue

            rateLimitTimer
        True -> rateLimitTimer

privmsg :: Text -> Text -> StatefulIRC BotState ()
privmsg chan msg = do
    s <- state

    msgsSent' <- readTVarIOL (s^.msgsSent)

    let msg' = RawMsg ("PRIVMSG " <> chan <> " :" <> msg)

    case (msgsSent' < 20) of
        True -> send msg'
        False -> addToQueue msg'

announce :: Text -> StatefulIRC BotState ()
announce msg = do
    s <- state
    privmsg (s^.config.channel) msg

replyTo :: Maybe Text -> Text -> StatefulIRC BotState ()
replyTo user msg = do
    s <- state
    privmsg (s^.config.channel) msg'

    where
        msg' = case user of
            Just user' -> "@" <> user' <> " " <> msg
            Nothing -> msg