{-# LANGUAGE TupleSections #-}
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
    )
where

import           Control.Concurrent.STM         ( TChan
                                                , TVar
                                                , newTChan
                                                , newTChanIO
                                                , newTVarIO
                                                , readTVarIO
                                                )
import           Control.Lens
import           Control.Monad                  ( forM
                                                , mapM
                                                , unless
                                                )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Data.Aeson                     ( eitherDecode )
import qualified Data.ByteString.Lazy          as BytesL
import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict           as HashMap
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import           Prelude                 hiding ( init )
import           System.Directory               ( doesFileExist )
import           System.Exit                    ( die )
import           Text.Megaparsec                ( errorBundlePretty
                                                , runParser
                                                )

import           Bot.Config                     ( Config )
import           Command.Info                   ( CommandInfo(..)
                                                , aliases
                                                , dynCmdInfo
                                                , name
                                                )
import           Command.Parser                 ( commandFromNameP )
import qualified Command.Permissions           as Perms
import           Command.Type                   ( Command )
import qualified Command.Type                  as Cmd
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

initConfig :: MonadIO io => io Config
initConfig = liftIO $ do
    bytes <- BytesL.readFile Paths.config
    either die pure $ eitherDecode bytes

initDynamicCmds :: MonadIO io => io (HashMap Text Command)
initDynamicCmds = liftIO $ do
    dynamicCmdsFileExists <- doesFileExist Paths.dynamicCmds
    unless dynamicCmdsFileExists $ BytesL.writeFile Paths.dynamicCmds "{}"
    bytes      <- BytesL.readFile Paths.dynamicCmds
    dynCmdsMap <- either die pure $ eitherDecode bytes
    pure $ fmap Cmd.Dynamic dynCmdsMap

initCmdToInfo
    :: MonadIO io => HashMap Text Command -> io (HashMap Command CommandInfo)
initCmdToInfo dynCmdsMap = liftIO $ do
    bytes        <- BytesL.readFile Paths.cmds
    cmdInfos     <- either die pure $ eitherDecode bytes
    cmdInfoPairs <- forM cmdInfos $ \info -> do
        let result = runParser commandFromNameP "" (info ^. name)
        either (die . errorBundlePretty) (\cmd -> pure (cmd, info)) result
    let dynCmdInfoPairs = fmap
            (\(txt, Cmd.Dynamic val) -> (Cmd.Dynamic val, dynCmdInfo txt))
            (HashMap.toList dynCmdsMap)
    pure $ HashMap.fromList (cmdInfoPairs <> dynCmdInfoPairs)

initTextToCmd :: [(Command, CommandInfo)] -> HashMap Text Command
initTextToCmd = HashMap.fromList . concatMap toNameCmdPairs
  where
    cmdNames :: CommandInfo -> [Text]
    cmdNames info = view name info : view aliases info

    toNameCmdPairs :: (Command, CommandInfo) -> [(Text, Command)]
    toNameCmdPairs (cmd, info) = fmap (, cmd) (cmdNames info)

init :: MonadIO io => io Env
init = liftIO $ do
    config'      <- initConfig
    dynamicCmds' <- initDynamicCmds >>= newTVarIO
    cmdToInfo'   <- readTVarIO dynamicCmds' >>= initCmdToInfo
    let textToCmd' = initTextToCmd (HashMap.toList cmdToInfo')
    outputChan' <- newTChanIO
    strings'    <- Text.readFile Paths.strings

    pure $ Env { _config      = config'
               , _outputChan  = outputChan'
               , _cmdToInfo   = cmdToInfo'
               , _textToCmd   = textToCmd'
               , _dynamicCmds = dynamicCmds'
               , _strings     = strings'
               }
