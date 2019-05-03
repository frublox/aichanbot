{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Bot.Type
    ( Bot
    , runBot
    ) where

import           Control.Concurrent.STM (atomically, modifyTVar', readTVarIO,
                                         writeTChan)
import           Control.Lens
import           Control.Monad          ((>=>))
import           Control.Monad.Catch    (MonadThrow)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader, ReaderT, runReaderT)
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy   as BytesL
import           Data.HashMap.Strict    ((!))
import qualified Data.HashMap.Strict    as HashMap
import           Data.Maybe             (mapMaybe)
import           Data.Text              (Text)
import           Text.Megaparsec        (parseMaybe)

import           Bot.Env
import           Bot.Monad
import           Command.Parser         (commandP)
import           Command.Type           (Command)
import qualified Command.Type           as Cmd
import           Lifted                 (atomicallyL, readTVarIOL)
import           Util                   (readAllTChan)

newtype Bot a = Bot { unBot :: ReaderT Env IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env,
        MonadThrow)

instance MonadBot Bot where
    getConfig = Bot (view config)
    getStrings = Bot (view botData)

    getCmds = Bot $ views cmdToInfo HashMap.keys
    getCmdInfo cmd = Bot $
        views cmdToInfo (\hm -> hm HashMap.! cmd)

    getDynCmd name = Bot $ do
        cmds <- view dynamicCmds >>= readTVarIOL
        pure (HashMap.lookup name cmds)
    addDynCmd name txt = Bot $ do
        var <- view dynamicCmds
        atomicallyL $ modifyTVar' var (HashMap.insert name (Cmd.Dynamic txt))
    delDynCmd name = Bot $ do
        var <- view dynamicCmds
        atomicallyL $ modifyTVar' var (HashMap.delete name)
    saveDynCmds = Bot $ do
        cmds <- view dynamicCmds >>= readTVarIOL
        let cmdsText = fmap show cmds
        liftIO $ BytesL.writeFile "dynamic_cmds.json" (encode cmdsText)

    sendMsg msg = Bot $ do
        chan <- view outputChan
        atomicallyL (writeTChan chan msg)
    checkMsgs = Bot $ do
        chan <- view outputChan
        atomicallyL (readAllTChan chan)

runBot ::  Env -> Bot a -> IO a
runBot env bot = runReaderT (unBot bot) env
