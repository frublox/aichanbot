{-# LANGUAGE TemplateHaskell #-}

module Irc.Types where

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

data EventHandler = EventHandler IrcEvent (Text -> Bot [Text])

data IrcBot = IrcBot
    { _onConnect     :: Bot [Text]
    , _eventHandlers :: [EventHandler]
    , _ircBotConfig  :: BotConfig
    , _ircBotState   :: BotState
    }
makeLenses ''IrcBot
