{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Bot.Util
    ( msgChannel
    , replyTo
    , replyHelpStr
    , lookUpStr
    , lookUpStrs
    , lookUpObj
    )
where

import           Control.Lens        ((^..))
import qualified Control.Lens        as Lens
import qualified Data.Aeson          as Aeson
import qualified Data.Aeson.Lens     as Aeson.Lens
import           Data.HashMap.Strict (HashMap, (!))
import           Data.Text           (Text)

import qualified Bot.Config          as Config
import           Bot.Monad           (MonadBot)
import qualified Bot.Monad           as Bot
import           Bot.Source          (Source)
import qualified Bot.Source          as Source
import qualified Command.Info        as Info
import           Command.Type        (Command)
import qualified Util

-- Sends a message to the current channel's chat.
msgChannel :: MonadBot m => Text -> m [Text]
msgChannel msg = do
    chan <- Lens.view Config.channel <$> Bot.getConfig
    pure ["PRIVMSG " <> chan <> " :" <> msg]

-- Replies to a message, @ing the source who sent it.
replyTo :: MonadBot m => Source -> Text -> m [Text]
replyTo src msg = msgChannel ("@" <> Source.toText src <> " " <> msg)

-- Replies to a message with the help string of a command.
replyHelpStr :: MonadBot m => Source -> Command -> m [Text]
replyHelpStr src cmd = do
    info <- Bot.getCmdInfo cmd
    replyTo src (Lens.view Info.help info)

{-@ type ListNE a = {v:[a] | 0 < len v} @-}

{-@ nestedLookup :: HashMap Text Aeson.Value -> ListNE Text -> Aeson.Value @-}
nestedLookup obj [key] = obj ! key
nestedLookup obj (key : keys) =
    nestedLookup (Lens.view Aeson.Lens._Object (obj ! key)) keys

-- Performs a nested lookup on the Bot's strings JSON.
lookUp :: MonadBot m => [Text] -> m Aeson.Value
lookUp path = do
    strs <- Bot.getStrings
    let obj = Lens.view Aeson.Lens._Object strs
    pure $ nestedLookup obj path

lookUpStr :: MonadBot m => [Text] -> m Text
lookUpStr path = Lens.view Aeson.Lens._String <$> lookUp path

lookUpStrs :: MonadBot m => [Text] -> m [Text]
lookUpStrs path = (^.. Aeson.Lens.values . Aeson.Lens._String) <$> lookUp path

lookUpObj :: MonadBot m => [Text] -> m (HashMap Text Aeson.Value)
lookUpObj path = Lens.view Aeson.Lens._Object <$> lookUp path
