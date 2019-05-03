{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Bot.Env
    ( Env
    , config
    , outputChan
    , cmdToInfo
    , dynamicCmds
    , botData

    , init
    ) where

import           Control.Concurrent.STM (TChan, TVar, newTChan, newTChanIO,
                                         newTVarIO)
import           Control.Lens
import           Control.Monad          (forM, mapM)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson             (eitherDecode)
import qualified Data.ByteString.Lazy   as BytesL
import           Data.HashMap.Strict    (HashMap)
import qualified Data.HashMap.Strict    as HashMap
import           Data.Ini
import           Data.Text              (Text)
import qualified Data.Text              as Text
import qualified Data.Text.IO           as Text
import           Prelude                hiding (init)
import           System.Exit            (die)
import           Text.Megaparsec        (runParser, showErrorComponent)

import           Bot.Config             (Config)
import           Command.Info           (CommandInfo (..), name)
import           Command.Parser         (commandP)
import qualified Command.Permissions    as Perms
import           Command.Type           (Command)
import qualified Command.Type           as Cmd

data Env = Env
    { _config      :: Config
    , _outputChan  :: TChan Text
    , _cmdToInfo   :: HashMap Command CommandInfo
    , _dynamicCmds :: TVar (HashMap Text Command)
    , _botData     :: Text
    }
makeLenses ''Env

init :: MonadIO io => io Env
init = liftIO $ do
    (config', cmdToInfo', dynamicCmds') <- do
        let jsonFiles = ["config.json", "cmds.json", "dynamic_cmds.json"]
        [configJson, cmdInfosJson, dynCmdsJson] <- mapM BytesL.readFile jsonFiles

        (config', cmdInfos, dynCmdsMap) <- either die pure $ do
            config' <- eitherDecode configJson
            cmdInfos <- eitherDecode cmdInfosJson
            dynCmds <- eitherDecode dynCmdsJson
            pure (config', cmdInfos, dynCmds)

        cmdInfoPairs <- forM (cmdInfos) $ \info -> do
            let result = runParser commandP "" (info^.name)
            either (die . show) (\cmd -> pure (cmd, info)) result

        let dynCmds = HashMap.keys (dynCmdsMap)
        let dynCmdInfoPairs = fmap (\txt -> (Cmd.Dynamic txt, dynCmdInfo txt)) dynCmds

        let cmdToInfo' = HashMap.fromList (cmdInfoPairs <> dynCmdInfoPairs)

        (,,) <$> pure config' <*> pure cmdToInfo' <*> newTVarIO (fmap Cmd.Dynamic dynCmdsMap)

    outputChan' <- newTChanIO
    botData' <- Text.readFile "bot.json"

    pure $ Env
        { _config = config'
        , _outputChan = outputChan'
        , _cmdToInfo = cmdToInfo'
        , _dynamicCmds = dynamicCmds'
        , _botData = botData'
        }

    where
        dynCmdInfo :: Text -> CommandInfo
        dynCmdInfo txt = CommandInfo
            { _name = txt
            , _aliases = []
            , _permissions = Perms.Anyone
            , _help = "Usage !" <> txt <> " [optional username, w/ or w/out @]"
            }