{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Bot.Types where

import           Data.Aeson
import qualified Data.ByteString.Lazy   as BytesL
import           Data.Ini
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as Map
import           Data.Text              (Text)

import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader
import           Control.Monad.State

import           Command.Types          (CommandInfo)
import           Lifted                 (atomicallyL)


data BotConfig = BotConfig
    { _botNick    :: Text
    , _botPass    :: Text
    , _channel    :: Text
    , _staticCmds :: Map Text CommandInfo
    , _outputChan :: TChan Text
    }
makeLenses ''BotConfig

initBotConfig :: MonadIO io => Ini -> io (Either String BotConfig)
initBotConfig ini = do
    staticCmds' <- liftIO $ do
        bytes <- BytesL.readFile "commands.json"
        return (eitherDecode bytes)

    outputChan' <- Right <$> atomicallyL newTChan

    return $ BotConfig
        <$> lookupValue "config" "nick" ini
        <*> lookupValue "config" "pass" ini
        <*> lookupValue "config" "channel" ini
        <*> staticCmds'
        <*> outputChan'

newtype BotState = BotState
    { _dynamicCmds :: Map Text Text
    }
makeLenses ''BotState

initBotState :: BotState
initBotState = BotState
    { _dynamicCmds = Map.empty
    }

newtype Bot a = Bot { unBot :: ReaderT BotConfig (StateT BotState IO) a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadState BotState,
        MonadReader BotConfig, MonadThrow)

runBot :: Bot a -> BotConfig -> BotState -> IO a
runBot bot botConf botState = do
    let unReader = runReaderT (unBot bot) botConf
    evalStateT unReader botState
