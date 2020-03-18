{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Bot.Type
    ( Bot
    , runBot
    )
where

import           Control.Applicative            ( (<|>) )
import           Control.Concurrent.STM         ( atomically
                                                , modifyTVar'
                                                , readTVarIO
                                                , writeTChan
                                                )
import           Control.Lens
import           Control.Monad                  ( (>=>) )
import           Control.Monad.Catch            ( MonadThrow )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Control.Monad.Logger
import           Control.Monad.Reader           ( MonadReader
                                                , ReaderT
                                                , runReaderT
                                                )
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy          as BytesL
import           Data.HashMap.Strict            ( (!) )
import qualified Data.HashMap.Strict           as HashMap
import           Data.Maybe                     ( mapMaybe )
import           Data.Text                      ( Text )
import           System.Random                  ( randomRIO )
import           Text.Megaparsec                ( parseMaybe )

import           Bot.Env
import           Bot.Monad
import           Command.Info                   ( dynCmdInfo )
import           Command.Parser                 ( commandP )
import           Command.Type                   ( Command )
import qualified Command.Type                  as Cmd
import           Lifted                         ( atomicallyL
                                                , readTVarIOL
                                                )
import           Random.Monad                   ( MonadRandom
                                                , randomR
                                                )
import qualified Paths
import           Util                           ( readAllTChan )

newtype Bot a = Bot { unBot :: LoggingT (ReaderT Env IO) a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env,
        MonadThrow, MonadLogger)

instance MonadBot Bot where
    getConfig  = Bot (view config)
    getStrings = Bot (view strings)

    getCmds    = Bot $ views cmdToInfo HashMap.keys
    resolveCmd txt = Bot $ do
        cmds    <- view textToCmd
        dynCmds <- view dynamicCmds >>= readTVarIOL
        pure (HashMap.lookup txt cmds <|> HashMap.lookup txt dynCmds)
    getCmdInfo cmd = Bot $ case cmd of
        (Cmd.Dynamic txt) -> pure (dynCmdInfo txt)
        _                 -> views cmdToInfo (! cmd)

    getDynCmd name = Bot $ do
        cmds <- view dynamicCmds >>= readTVarIOL
        pure (cmds HashMap.! name)
    getDynCmdNames = Bot $ HashMap.keys <$> (view dynamicCmds >>= readTVarIOL)
    addDynCmd name txt = Bot $ do
        var <- view dynamicCmds
        atomicallyL $ modifyTVar' var (HashMap.insert name (Cmd.Dynamic txt))
    delDynCmd name = Bot $ do
        var <- view dynamicCmds
        atomicallyL $ modifyTVar' var (HashMap.delete name)
    saveDynCmds = Bot $ do
        cmds <- view dynamicCmds >>= readTVarIOL
        let cmdsText = fmap Cmd.toText cmds
        liftIO $ BytesL.writeFile Paths.dynamicCmds (encode cmdsText)

    sendMsg msg = Bot $ do
        chan <- view outputChan
        atomicallyL (writeTChan chan msg)
    checkMsgs = Bot $ do
        chan <- view outputChan
        atomicallyL (readAllTChan chan)

instance MonadRandom Bot where
    randomR = Bot . liftIO . randomRIO

runBot :: Env -> Bot a -> IO a
runBot env bot = runReaderT (runStdoutLoggingT (unBot bot)) env
