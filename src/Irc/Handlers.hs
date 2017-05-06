{-# LANGUAGE OverloadedStrings #-}

module Irc.Handlers where

import           Data.Monoid ((<>))
import qualified Data.Text   as Text

import           Bot.Actions (send)
import           Irc.Types

pingHandler :: EventHandler
pingHandler = EventHandler EPing $ \msg ->
    send ("PONG :" <> Text.drop 6 msg)
