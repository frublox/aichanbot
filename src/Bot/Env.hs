{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Bot.Env
    ( Env
    , config
    , outputChan
    , cmdToInfo
    , textToCmd
    , dynamicCmds
    , strings

    , init
    ) where

import           Control.Concurrent.STM (TChan, TVar, newTChan, newTChanIO,
                                         newTVarIO)
import           Control.Lens
import           Control.Monad          (forM, mapM, unless)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson             (eitherDecode)
import qualified Data.ByteString.Lazy   as BytesL
import           Data.HashMap.Strict    (HashMap)
import qualified Data.HashMap.Strict    as HashMap
import           Data.Text              (Text)
import qualified Data.Text              as Text
import qualified Data.Text.IO           as Text
import           Prelude                hiding (init)
import           System.Directory       (doesFileExist)
import           System.Exit            (die)
import           Text.Megaparsec        (errorBundlePretty, runParser)

import           Bot.Config             (Config)
import           Command.Info           (CommandInfo (..), aliases, dynCmdInfo,
                                         name)
import           Command.Parser         (commandFromNameP)
import qualified Command.Permissions    as Perms
import           Command.Type           (Command)
import qualified Command.Type           as Cmd
import qualified Paths

data Env = Env
    { _config      :: Config
    , _outputChan  :: TChan Text
    , _cmdToInfo   :: HashMap Command CommandInfo
    , _textToCmd   :: HashMap Text Command
    , _dynamicCmds :: TVar (HashMap Text Command)
    , _strings     :: Text
    }
makeLenses ''Env

init :: MonadIO io => io Env
init = liftIO $ do
    dynamicCmdsFileExists <- doesFileExist Paths.dynamicCmds
    unless dynamicCmdsFileExists $
        BytesL.writeFile Paths.dynamicCmds "{}"

    (config', cmdToInfo', textToCmd', dynamicCmds') <- do
        let jsonFiles = [Paths.config, Paths.cmds, Paths.dynamicCmds]
        [configJson, cmdInfosJson, dynCmdsJson] <- mapM BytesL.readFile jsonFiles

        (config', cmdInfos, dynCmdsMap) <- either die pure $ do
            config' <- eitherDecode configJson
            cmdInfos <- eitherDecode cmdInfosJson
            dynCmds <- eitherDecode dynCmdsJson
            pure (config', cmdInfos, dynCmds)

        cmdInfoPairs <- forM cmdInfos $ \info -> do
            let result = runParser commandFromNameP "" (info^.name)
            either (die . errorBundlePretty) (\cmd -> pure (cmd, info)) result

        let dynCmdInfoPairs = fmap 
                (\(txt, val) -> (Cmd.Dynamic val, dynCmdInfo txt)) 
                (HashMap.toList (dynCmdsMap))

        let textToCmd' = HashMap.fromList $
                concat $ (flip map) cmdInfoPairs $
                    \(cmd, info) ->
                        let names = view name info : view aliases info
                        in  fmap (\n -> (n, cmd)) names

        let cmdToInfo' = HashMap.fromList (cmdInfoPairs)

        (,,,)
            <$> pure config'
            <*> pure cmdToInfo'
            <*> pure textToCmd'
            <*> newTVarIO (fmap Cmd.Dynamic dynCmdsMap)

    outputChan' <- newTChanIO
    strings' <- Text.readFile Paths.strings

    pure $ Env
        { _config = config'
        , _outputChan = outputChan'
        , _cmdToInfo = cmdToInfo'
        , _textToCmd = textToCmd'
        , _dynamicCmds = dynamicCmds'
        , _strings = strings'
        }
