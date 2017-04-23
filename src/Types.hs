{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Types where

import           Data.Char              (isSpace)
import           Data.List              (words)
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as Map
import           Data.Text              (Text, pack)

import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader
import           Control.Monad.State    hiding (state)

import           Network.IRC.Client
import           Text.Parsec            hiding (putState)
import           Text.Parsec.Text

import           Lifted

makeLenses ''InstanceConfig
makeLenses ''Event
makeLenses ''IRCState

data BotConfig = BotConfig
    { _botNick :: Text
    , _pass    :: Text
    , _channel :: Text
    }
makeLenses ''BotConfig

data BotState = BotState
    { _config      :: BotConfig
    , _timeLeft    :: TVar Int -- Time left in seconds until rate limit resets
    , _msgsSent    :: TVar Int -- Number of msgs sent since last rate limit reset
    , _msgQueue    :: TChan UnicodeMessage
    , _dynamicCmds :: TVar (Map Text Text)
    }
makeLenses ''BotState

initBotState :: MonadIO io => BotConfig -> io BotState
initBotState botConf = atomicallyL $ do
    timeLeft' <- newTVar 20
    msgsSent' <- newTVar 0
    msgQueue' <- newTChan
    dynamicCmds' <- newTVar (Map.empty)

    return $ BotState
        { _config = botConf
        , _timeLeft = timeLeft'
        , _msgsSent = msgsSent'
        , _msgQueue = msgQueue'
        , _dynamicCmds = dynamicCmds'
        }

type Bot = ReaderT (IRCState BotState) IO

data Command
    = CmdUnknown
    | CmdCommands
    | CmdHi (Maybe Text)
    | CmdBye (Maybe Text)
    | CmdAdd Text Text
    | CmdRemove Text
    | CmdDynamic Text

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
            (user:_) -> (return . CmdHi . Just . toText) user
            _        -> return (CmdHi Nothing)

        "bye" -> case args of
            (user:_) -> (return . CmdBye . Just . toText) user
            _        -> return (CmdBye Nothing)

        "add" -> case args of
            (cmdName:cmdText:_) -> return $ CmdAdd (toText cmdName) (toText cmdText)
            _                   -> parserFail "!add takes two arguments"

        "remove" -> case args of
            (cmdName:_) -> return $ CmdRemove (toText cmdName)
            _           -> parserFail "!remove takes one argument"

        _ -> return CmdUnknown

    where
        toText = pack . stripPrefix
        stripPrefix s = case s of
            ('@':s') -> s'
            _        -> s

commandName :: Parser Text
commandName = do
    char '!'
    cmd <- many alphaNum
    return (pack cmd)
