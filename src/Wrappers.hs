{-# LANGUAGE OverloadedStrings #-}

module Wrappers where

import           Data.Monoid        ((<>))
import           Data.Text          (Text)

import           Control.Lens

import           Network.IRC.Client

import           Bot

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
