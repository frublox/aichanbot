{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where

import Data.Text (Text, pack)
import Data.Char (isSpace)
import Data.List (words)

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
    | CmdHi (Maybe Text)

command :: Parser Command
command = do
    char '!'
    cmd <- many alphaNum
    args <- fmap words (manyTill anyChar eof)

    case cmd of
        "commands" -> return CmdCommands
        "list" -> return CmdCommands
        "hi" -> case args of
            [] -> (return . CmdHi) Nothing
            (user:_) -> (return . CmdHi . Just . pack . removePrefix) user
        _ -> return CmdUnknown

    where
        removePrefix s = case s of
            ('@':s') -> s'
            _ -> s