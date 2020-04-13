{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}

module Bot.Env
    ( Env
    , config
    , cmdToInfo
    , textToCmd
    , dynamicCmds
    , strings

    , dynamicCmdsPath
    , stringsPath

    , init
    )
where

import qualified Control.Concurrent.STM as STM
import qualified Control.Lens           as Lens
import           Control.Monad          (forM, mapM, unless)
import qualified Data.Aeson             as Aeson
import qualified Data.ByteString.Lazy   as Bytes
import           Data.HashMap.Strict    (HashMap)
import qualified Data.HashMap.Strict    as HashMap
import           Data.Text              (Text)
import qualified Data.Text              as Text
import qualified Data.Text.IO           as Text.IO
import           Prelude                hiding (init)
import qualified System.Directory
import qualified System.Exit
import qualified Text.Megaparsec        as Megaparsec

import           Bot.Config             (Config)
import           Command.Info           (CommandInfo (..), aliases, dynCmdInfo,
                                         name)
import           Command.Parser         (commandFromNameP)
import           Command.Type           (Command)
import qualified Command.Type           as Cmd

data Env = Env
    { _config      :: Config
    , _cmdToInfo   :: HashMap Command CommandInfo
    , _textToCmd   :: HashMap Text Command
    , _dynamicCmds :: STM.TVar (HashMap Text Command)
    , _strings     :: STM.TVar Text
    }
Lens.makeLenses ''Env

configPath :: FilePath
configPath = "config.json"

cmdsPath :: FilePath
cmdsPath = "data/cmds.json"

dynamicCmdsPath :: FilePath
dynamicCmdsPath = "data/dynamic_cmds.json"

stringsPath :: FilePath
stringsPath = "data/strings.json"

init :: IO Env
init = do
    config' <- Bytes.readFile configPath >>= decodeOrDie

    dynamicCmds' <- STM.newTVarIO =<< do
        fileExists <- System.Directory.doesFileExist cmdsPath
        unless fileExists $
            Bytes.writeFile dynamicCmdsPath "{}"
        dynCmdsMap <- Bytes.readFile dynamicCmdsPath >>= decodeOrDie
        pure $ fmap Cmd.Dynamic dynCmdsMap

    cmdToInfo' <- do
        infos <- Bytes.readFile cmdsPath >>= decodeOrDie
        infoPairs <- forM infos $ \info -> do
            let result = Megaparsec.runParser commandFromNameP "" (Lens.view name info)
            case result of
                Left err -> System.Exit.die (Megaparsec.errorBundlePretty err)
                Right cmd -> pure (cmd, info)
        pure $ HashMap.fromList infoPairs

    let cmdNames info = Lens.view name info : Lens.view aliases info
    let toNameCmdPairs (cmd, info) = fmap (, cmd) (cmdNames info)
    let textToCmd' = HashMap.fromList . concatMap toNameCmdPairs . HashMap.toList $ cmdToInfo'
    
    strings' <- Text.IO.readFile stringsPath >>= STM.newTVarIO

    pure $ Env { _config      = config'
               , _cmdToInfo   = cmdToInfo'
               , _textToCmd   = textToCmd'
               , _dynamicCmds = dynamicCmds'
               , _strings     = strings'
               }

    where
        decodeOrDie :: Aeson.FromJSON a => Bytes.ByteString -> IO a
        decodeOrDie bytes = case Aeson.eitherDecode' bytes of
            Left err  -> System.Exit.die err
            Right val -> pure val
