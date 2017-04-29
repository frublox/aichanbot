{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module IrcLenses where

import           Network.IRC.Client (Event, IRCState, InstanceConfig)

import           Control.Lens

makeLensesFor
    [("_password", "password"), ("_eventHandlers", "eventHandlers")]
    ''InstanceConfig
makeLenses ''Event
makeLenses ''IRCState




