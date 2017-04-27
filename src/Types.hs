{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Types where

import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as Map
import           Data.Text              (Text)
import qualified Data.Text              as Text

import           Control.Concurrent.STM
import           Control.Lens           hiding (noneOf)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader

import           Network.IRC.Client
import           Text.Megaparsec
import           Text.Megaparsec.Text

import           Lifted

makeLensesFor
    [("_password", "password"), ("_eventHandlers", "eventHandlers")]
    ''InstanceConfig
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
    dynamicCmds' <- newTVar Map.empty

    return BotState
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

command :: ParsecT Dec Text Bot Command
command = do
    char '!'
    cmdName <- many alphaNumChar

    dynCmd <- (lift . lookupDynCmd . Text.pack) cmdName

    case dynCmd of
        Just _ -> (return . CmdDynamic . Text.pack) cmdName
        Nothing -> do
            args <- space *> many (quotedStr <|> word <* space)

            case cmdName of
                "commands" -> return CmdCommands
                "list" -> return CmdCommands
                "cmds" -> return CmdCommands

                "hi" -> case args of
                    (user:_) -> (return . CmdHi . Just . Text.pack . stripAt) user
                    _        -> return (CmdHi Nothing)

                "bye" -> case args of
                    (user:_) -> (return . CmdBye . Just . Text.pack . stripAt) user
                    _        -> return (CmdBye Nothing)

                "add" -> case args of
                    (name:cmdText:_) -> return $ CmdAdd (Text.pack name) (Text.pack cmdText)
                    _                   -> fail "!add takes two arguments"

                "remove" -> case args of
                    (name:_) -> (return . CmdRemove . Text.pack) name
                    _        -> fail "!remove takes one argument"

                _ -> return CmdUnknown

    where
        stripAt s = case s of
            ('@':s') -> s'
            _        -> s

        lookupDynCmd :: Text -> Bot (Maybe Command)
        lookupDynCmd cmdName = do
            s <- state
            cmds <- readTVarIOL (s^.dynamicCmds)

            case Map.lookup cmdName cmds of
                Nothing -> return Nothing
                _       -> return $ Just (CmdDynamic cmdName)

quotedStr :: Monad m => ParsecT Dec Text m String
quotedStr = char '\"' *> many (noneOf ['\"']) <* char '\"'

word :: Monad m => ParsecT Dec Text m String
word = someTill anyChar (skipSome (oneOf [' ']) <|> eof)

commandName :: Parser Text
commandName = Text.pack <$> (char '!' *> many alphaNumChar)
