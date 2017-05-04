{-# LANGUAGE OverloadedStrings #-}

module Bot.Actions where

import           Data.Monoid            ((<>))
import           Data.Text              (Text)

import           Control.Concurrent.STM
import           Control.Lens

import           Bot.Types
import           Lifted                 (atomicallyL)

send :: Text -> Bot ()
send msg = do
    output <- view outputChan
    atomicallyL (writeTChan output msg)

replyTo :: Text -> Text -> Bot ()
replyTo user msg = send ("@" <> user <> " " <> msg)
