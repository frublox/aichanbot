module Bot.Monad
    ( MonadBot
    , getConfig
    , getStrings

    , getCmds
    , resolveCmd
    , getCmdInfo

    , getDynCmd
    , getDynCmdNames
    , addDynCmd
    , delDynCmd
    , saveDynCmds

    , checkMsgs
    , sendMsg

    , randomR
    ) where

import           Data.HashMap.Strict (HashMap)
import           Data.Text           (Text)
import           System.Random       (Random)

import           Bot.Config          (Config)
import           Command.Info        (CommandInfo)
import           Command.Type        (Command)

class (Monad m) => MonadBot m where
    getConfig :: m Config
    getStrings :: m Text

    getCmds :: m [Command]
    resolveCmd :: Text -> m (Maybe Command)
    getCmdInfo :: Command -> m CommandInfo

    getDynCmd :: Text -> m Command
    getDynCmdNames :: m [Text]
    addDynCmd :: Text -> Text -> m ()
    delDynCmd :: Text -> m ()
    saveDynCmds :: m ()

    checkMsgs :: m [Text]
    sendMsg :: Text -> m ()

    randomR :: Random a => (a, a) -> m a
