{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where

import Data.Text (Text, pack)
import Data.Char (isSpace)
import Data.List (words)

import Control.Monad.IO.Class (MonadIO)
import Control.Concurrent.STM
import Control.Lens

import Text.Parsec
import Text.Parsec.Text
import Network.IRC.Client

import Lifted

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
    , _timeLeft :: TVar Int -- Time left in seconds until rate limit resets
    , _msgsSent :: TVar Int -- Number of msgs sent since last rate limit reset
    , _msgQueue :: TChan UnicodeMessage
    }
makeLenses ''BotState

initBotState :: MonadIO io => BotConfig -> io BotState
initBotState conf = do
    timeLeft' <- atomicallyL (newTVar 20)
    msgsSent' <- atomicallyL (newTVar 0)
    msgQueue' <- atomicallyL newTChan

    return $ BotState
        { _config = conf
        , _timeLeft = timeLeft'
        , _msgsSent = msgsSent'
        , _msgQueue = msgQueue'
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