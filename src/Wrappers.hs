{-# LANGUAGE OverloadedStrings #-}

module Wrappers where

import           Data.Monoid               ((<>))
import           Data.Text                 (Text)

import           Control.Concurrent.Lifted (threadDelay)
import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Control.Monad.Reader
import           Control.Monad.State       hiding (state)

import           Network.IRC.Client
import qualified Network.IRC.Client        as Irc

import           Lifted                    (atomicallyL, readTVarIOL)
import           Types

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
            Nothing   -> msg
