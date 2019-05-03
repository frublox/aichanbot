module Bot.Monad
    ( MonadBot
    , getConfig
    , getStrings

    , getCmds
    , getCmdInfo

    , getDynCmd
    , addDynCmd
    , delDynCmd
    , saveDynCmds

    , checkMsgs
    , sendMsg
    ) where

import           Data.HashMap.Strict (HashMap)
import           Data.Text           (Text)

import           Bot.Config          (Config)
import           Command.Info        (CommandInfo)
import           Command.Type        (Command)

class (Monad m) => MonadBot m where
    getConfig :: m Config
    getStrings :: m Text

    getCmds :: m [Command]
    getCmdInfo :: Command -> m CommandInfo

    getDynCmd :: Text -> m (Maybe Command)
    addDynCmd :: Text -> Text -> m ()
    delDynCmd :: Text -> m ()
    saveDynCmds :: m ()

    checkMsgs :: m [Text]
    sendMsg :: Text -> m ()
