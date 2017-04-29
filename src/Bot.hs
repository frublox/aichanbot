{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Bot where

import           Data.Aeson
import qualified Data.ByteString.Lazy   as Bytes
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as Map
import           Data.Text              (Text)
import qualified Data.Text              as Text

import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader
import           Network.IRC.Client

import           System.Exit            (die)

import           Command                (CommandInfo)
import           Lifted                 (atomicallyL)

data BotConfig = BotConfig
    { _botNick :: Text
    , _pass    :: Text
    , _channel :: Text
    }
makeLenses ''BotConfig

data BotState = BotState
    { _config      :: BotConfig
    , _staticCmds  :: Map Text CommandInfo
    , _dynamicCmds :: TVar (Map Text Text)
    }
makeLenses ''BotState

initBotState :: MonadIO io => BotConfig -> io BotState
initBotState botConf = do
    staticCmds' <- liftIO $ do
        bytes <- Bytes.readFile "commands.json"
        either die return (eitherDecode bytes)

    dynamicCmds' <- atomicallyL (newTVar Map.empty)

    return BotState
        { _config = botConf
        , _staticCmds = staticCmds'
        , _dynamicCmds = dynamicCmds'
        }

type Bot = ReaderT (IRCState BotState) IO
