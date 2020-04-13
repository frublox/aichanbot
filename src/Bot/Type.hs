{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Bot.Type
    ( Bot
    , runBot
    )
where

import           Control.Applicative    ((<|>))
import qualified Control.Concurrent.STM as STM
import           Control.Lens
import           Control.Monad.Catch    (MonadThrow)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Logger   (LoggingT, MonadLogger,
                                         runStdoutLoggingT)
import           Control.Monad.Reader   (MonadReader, ReaderT, runReaderT)
import qualified Data.Aeson             as Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy   as Bytes
import           Data.HashMap.Strict    ((!))
import qualified Data.HashMap.Strict    as HashMap
import           Data.Maybe             (mapMaybe)
import           Data.Text              (Text)
import qualified Data.Text.IO           as Text.IO
import           System.Random          (randomRIO)
import           Text.Megaparsec        (parseMaybe)

import           Bot.Env                (Env)
import qualified Bot.Env                as Env
import qualified Bot.Monad
import           Command.Info           (dynCmdInfo)
import           Command.Parser         (commandP)
import           Command.Type           (Command)
import qualified Command.Type           as Cmd
import           Lifted                 (atomicallyL, readTVarIOL)
import           Random.Monad           (MonadRandom, randomR)
import qualified Util

newtype Bot a = Bot { unBot :: LoggingT (ReaderT Env IO) a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env,
        MonadThrow, MonadLogger)

instance Bot.Monad.MonadBot Bot where
    getConfig  = Bot (view Env.config)
    getStrings = Bot $ do
        var <- view Env.strings
        liftIO $ STM.readTVarIO var

    getCmds    = Bot $ views Env.cmdToInfo HashMap.keys
    resolveCmd txt = Bot $ do
        cmds    <- view Env.textToCmd
        dynCmds <- view Env.dynamicCmds >>= readTVarIOL
        pure (HashMap.lookup txt cmds <|> HashMap.lookup txt dynCmds)
    getCmdInfo cmd = Bot $ case cmd of
        (Cmd.Dynamic txt) -> pure (dynCmdInfo txt)
        _                 -> views Env.cmdToInfo (! cmd)

    reload = Bot $ do
        contents <- liftIO $ Text.IO.readFile Env.stringsPath
        var <- view Env.strings
        atomicallyL $ 
            STM.writeTVar var contents

    getDynCmd name = Bot $ do
        cmds <- view Env.dynamicCmds >>= readTVarIOL
        pure (cmds HashMap.! name)
    getDynCmdNames = Bot $ do
        cmds <- view Env.dynamicCmds >>= readTVarIOL
        pure (HashMap.keys cmds)
    addDynCmd name txt = Bot $ do
        var <- view Env.dynamicCmds
        atomicallyL $
            STM.modifyTVar' var (HashMap.insert name (Cmd.Dynamic txt))
    delDynCmd name = Bot $ do
        var <- view Env.dynamicCmds
        atomicallyL $
            STM.modifyTVar' var (HashMap.delete name)
    saveDynCmds = Bot $ do
        cmds <- view Env.dynamicCmds >>= readTVarIOL
        let cmdsText = fmap Cmd.toText cmds
        liftIO $ Bytes.writeFile Env.dynamicCmdsPath (Aeson.encode cmdsText)

instance MonadRandom Bot where
    randomR = Bot . liftIO . randomRIO

runBot :: Env -> Bot a -> IO a
runBot env bot = runReaderT (runStdoutLoggingT (unBot bot)) env
