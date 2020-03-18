{-# LANGUAGE OverloadedStrings #-}

module Bot.Util
    ( privMsg
    , replyTo
    , replyHelpStr
    )
where

import           Control.Lens
import           Data.Text                      ( Text )

import           Bot.Source                     ( Source )
import qualified Bot.Source                    as Source
import qualified Bot.Config                    as Config
import           Bot.Monad
import qualified Command.Info                  as Info
import           Command.Type                   ( Command )

privMsg :: MonadBot m => Text -> m ()
privMsg msg = do
    chan <- view Config.channel <$> getConfig
    let ircMsg = "PRIVMSG " <> chan <> " :" <> msg
    sendMsg ircMsg

replyTo :: MonadBot m => Source -> Text -> m ()
replyTo src msg = privMsg ("@" <> Source.toText src <> " " <> msg)

replyHelpStr :: MonadBot m => Source -> Command -> m ()
replyHelpStr src cmd = do
    info <- getCmdInfo cmd
    replyTo src (view Info.help info)
