{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Types where

import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.ByteString.Lazy   as BytesL
import           Data.Ini
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as Map
import           Data.Monoid            ((<>))
import           Data.Text              (Text, unpack)

import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader
import           Control.Monad.State

import           Lifted                 (atomicallyL)

data BotData = BotData
    { _strings   :: Map Text Text
    , _responses :: Map Text Text
    }

data BotConfig = BotConfig
    { _botNick    :: Text
    , _botPass    :: Text
    , _channel    :: Text
    , _outputChan :: TChan Text
    , _commands   :: [Command]
    , _botData    :: BotData
    }

initBotConfig :: MonadIO io => [Command] -> Ini -> io (Either String BotConfig)
initBotConfig cmds ini = do
    botData' <- liftIO $ do
        bytes <- BytesL.readFile "bot.json"
        return (eitherDecode bytes)

    outputChan' <- Right <$> atomicallyL newTChan

    return $ BotConfig
        <$> lookupValue "config" "nick" ini
        <*> lookupValue "config" "pass" ini
        <*> lookupValue "config" "channel" ini
        <*> outputChan'
        <*> pure cmds
        <*> botData'

newtype BotState = BotState
    { _dynamicCmds :: Map Text Text
    }

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

data CmdPermissions
    = PermAnyone
    | PermModOnly
    deriving (Show, Eq, Ord)

instance FromJSON CmdPermissions where
    parseJSON = withText "Permission" $ \val ->
        case val of
            "anyone"  -> return PermAnyone
            "modonly" -> return PermModOnly
            perm      -> fail $ "Invalid permission: '" <> unpack perm <> "'"

instance ToJSON CmdPermissions where
    toJSON PermAnyone  = String "anyone"
    toJSON PermModOnly = String "modonly"

data CommandInfo = CommandInfo
    { _name        :: Text
    , _aliases     :: [Text]
    , _permissions :: CmdPermissions
    , _help        :: Text
    }

data Command = Command
    { _info  :: CommandInfo
    , runCmd :: Text -> [Text] -> Invocation
    }

data Invocation = Invocation
    { _invocOf   :: Command
    , _cmdSource :: Text
    , _arguments :: [Text]
    , _runResult :: Bot ()
    }

makeCommand ::
    (Text -> [Text] -> Bot ())
    -> CommandInfo
    -> Command
makeCommand cmdResult cmdInfo = Command
    cmdInfo
    $ \source args ->
        Invocation
            (makeCommand cmdResult cmdInfo)
            source args (cmdResult source args)

makeLenses ''BotData
makeLenses ''BotConfig
makeLenses ''BotState

makeLenses ''CommandInfo
makeLenses ''Command
makeLenses ''Invocation

$(deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''BotData)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''CommandInfo)
