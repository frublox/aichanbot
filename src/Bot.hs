{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Bot where

import           Data.Aeson
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Lazy   as BytesL
import           Data.Ini
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as Map
import           Data.Text              (Text)

import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader
import           Control.Monad.State

import           System.Exit            (die)

import           Command                (CommandInfo)
import           Lifted                 (atomicallyL)

import           Irc                    (runIrcBot)

data BotConfig = BotConfig
    { _botNick    :: Text
    , _botPass    :: Text
    , _channel    :: Text
    , _staticCmds :: Map Text CommandInfo
    , _inputChan  :: TChan Text
    , _outputChan :: TChan Text
    }
makeLenses ''BotConfig

initBotConfig :: MonadIO io => Ini -> TChan Text -> TChan Text -> io (Either String BotConfig)
initBotConfig ini input output = do
    staticCmds' <- liftIO $ do
        bytes <- BytesL.readFile "commands.json"
        either die return (eitherDecode bytes)

    BotConfig
        <$> lookupValue "config" "nick" ini
        <*> lookupValue "config" "pass" ini
        <*> lookupValue "config" "channel" ini
        <*> staticCmds'
        <*> input
        <*> output

newtype BotState = BotState
    { _dynamicCmds :: Map Text Text
    }
makeLenses ''BotState

initBotState :: BotState
initBotState = BotState
    { _dynamicCmds = Map.empty
    }

newtype Bot a = Bot { unBot :: MonadIO io => ReaderT BotConfig (StateT BotState io) a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadState BotState, MonadReader BotConfig)

runBot :: MonadIO io => Bot a -> BotConfig -> BotState -> io a
runBot bot botConf botState = do
    let unReader = runReaderT (unBot bot) botConf
    runStateT unReader botState

