{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where

import Data.Text (Text, pack)
import Data.Char (isSpace)
import Data.List (words)

import Control.Monad.Reader
import Control.Monad.State hiding (state)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Concurrent.STM
import Control.Lens

import Text.Parsec hiding (putState)
import Text.Parsec.Text
import Network.IRC.Client

import Lifted

makeLenses ''InstanceConfig
makeLenses ''Event
makeLenses ''IRCState

data BotConfig = BotConfig
    { _botNick :: Text
    , _pass :: Text
    , _channel :: Text
    }
makeLenses ''BotConfig

data BotState = BotState
    { _timeLeft :: TVar Int -- Time left in seconds until rate limit resets
    , _msgsSent :: TVar Int -- Number of msgs sent since last rate limit reset
    , _msgQueue :: TChan UnicodeMessage
    }
makeLenses ''BotState

initBotState :: MonadIO io => io BotState
initBotState = do
    timeLeft' <- atomicallyL (newTVar 20)
    msgsSent' <- atomicallyL (newTVar 0)
    msgQueue' <- atomicallyL newTChan

    return $ BotState
        { _timeLeft = timeLeft'
        , _msgsSent = msgsSent'
        , _msgQueue = msgQueue'
        }

type Bot a = ReaderT (IRCState IrcEnv) (StateT IrcEnv IO) a

data IrcEnv = IrcEnv
    { _config :: BotConfig
    , _botState :: BotState
    }
makeLenses ''IrcEnv

runBot :: Bot a -> StatefulIRC IrcEnv a
runBot bot = do
    s <- state
    ircS <- ircState

    let unReader = runReaderT bot ircS

    (result, s') <- liftIO (runStateT unReader s)
    putState s'

    return result

data Command
    = CmdUnknown
    | CmdCommands
    | CmdHi (Maybe Text)
    | CmdBye (Maybe Text)

command :: Parser Command
command = do
    char '!'
    cmd <- many alphaNum
    args <- fmap words (manyTill anyChar eof)

    case cmd of
        "commands" -> return CmdCommands
        "list" -> return CmdCommands
        "cmds" -> return CmdCommands

        "hi" -> case args of
            [] -> return (CmdHi Nothing)
            (user:_) -> (return . CmdHi . Just . toText) user

        "bye" -> case args of
            [] -> return (CmdBye Nothing)
            (user:_) -> (return . CmdBye . Just . toText) user

        _ -> return CmdUnknown

    where
        toText = pack . stripPrefix
        stripPrefix s = case s of
            ('@':s') -> s'
            _ -> s