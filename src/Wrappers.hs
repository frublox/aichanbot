{-# LANGUAGE OverloadedStrings #-}

module Wrappers where

import Data.Text (Text)
import Data.Monoid ((<>))

import Control.Lens

import Network.IRC.Client

import Types

privmsg :: Text -> Text -> StatefulIRC a ()
privmsg chan msg = send $ RawMsg ("PRIVMSG " <> chan <> " :" <> msg)

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