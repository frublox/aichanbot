{-# LANGUAGE OverloadedStrings #-}

module Bot.Util
    ( privMsg
    , replyTo
    , replyHelpStr
    )
where

import           Control.Lens
import           Data.Text                      ( Text )

import qualified Bot.Config                    as Config
import           Bot.Monad
import qualified Command.Info                  as Info
import           Command.Type                   ( Command )

privMsg :: MonadBot m => Text -> m ()
privMsg msg = do
    chan <- view Config.channel <$> getConfig
    let ircMsg = "PRIVMSG " <> chan <> " :" <> msg
    sendMsg ircMsg

replyTo :: MonadBot m => Text -> Text -> m ()
replyTo user msg = privMsg ("@" <> user <> " " <> msg)

replyHelpStr :: MonadBot m => Text -> Command -> m ()
replyHelpStr source cmd = do
    info <- getCmdInfo cmd
    replyTo source (view Info.help info)
