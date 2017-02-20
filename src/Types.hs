{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where

import Data.Text (Text)

import Control.Lens

import Text.Parsec
import Text.Parsec.Text
import Network.IRC.Client

makeLenses ''InstanceConfig
makeLenses ''Event

data BotConfig = BotConfig
    { _botNick :: Text
    , _pass :: Text
    , _channel :: Text
    }
makeLenses ''BotConfig

data BotState = BotState
    { _config :: BotConfig
    }
makeLenses ''BotState

initBotState :: BotConfig -> BotState
initBotState conf = BotState
    { _config = conf
    }

data Command
    = CmdUnknown
    | CmdCommands
    | CmdHi

command :: Parser Command
command = do
    char '!'
    cmd <- many alphaNum
    args <- many (many alphaNum)

    case cmd of
        "commands" -> return CmdCommands
        "list" -> return CmdCommands
        "hi" -> return CmdHi
        _ -> return CmdUnknown