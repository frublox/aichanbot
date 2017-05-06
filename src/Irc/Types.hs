{-# LANGUAGE TemplateHaskell #-}

module Irc.Types where

import           Control.Lens
import           Data.Text

import           Bot.Types

data IrcEvent
    = EPrivMsg
    | ENotice
    | ENick
    | EJoin
    | EPart
    | EQuit
    | EMode
    | ETopic
    | EInvite
    | EKick
    | EPing
    | EPong
    | ENumeric
    | ERawMsg
    deriving (Eq, Show)

data EventHandler = EventHandler IrcEvent (Text -> Bot ())

data IrcBot = IrcBot
    { _onConnect     :: Bot ()
    , _eventHandlers :: [EventHandler]
    , _ircBotConfig  :: BotConfig
    , _ircBotState   :: BotState
    }
makeLenses ''IrcBot
