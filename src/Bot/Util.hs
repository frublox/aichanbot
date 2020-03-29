{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Bot.Util
    ( msgChannel
    , replyTo
    , replyHelpStr
    , lookUpStr
    , lookUpStrs
    )
where

import           Control.Lens
import           Data.Text                      ( Text )
import           Data.Aeson
import qualified Data.Aeson.Lens               as Aeson
import           Data.HashMap.Strict            ( HashMap
                                                , (!)
                                                )

import           Bot.Source                     ( Source )
import qualified Bot.Source                    as Source
import qualified Bot.Config                    as Config
import           Bot.Monad
import qualified Command.Info                  as Info
import           Command.Type                   ( Command )
import qualified Util

-- Sends a message to the current channel's chat.
msgChannel :: MonadBot m => Text -> m ()
msgChannel msg = do
    chan <- view Config.channel <$> getConfig
    sendMsg ("PRIVMSG " <> chan <> " :" <> msg)

-- Replies to a message, @ing the source who sent it.
replyTo :: MonadBot m => Source -> Text -> m ()
replyTo src msg = msgChannel ("@" <> Source.toText src <> " " <> msg)

-- Replies to a message with the help string of a command.
replyHelpStr :: MonadBot m => Source -> Command -> m ()
replyHelpStr src cmd = do
    info <- getCmdInfo cmd
    replyTo src (view Info.help info)

{-@ type ListNE a = {v:[a] | 0 < len v} @-}

{-@ nestedLookup :: HashMap Text Value -> ListNE Text -> Value @-}
nestedLookup obj [key] = obj ! key
nestedLookup obj (key : keys) =
    nestedLookup (view Aeson._Object (obj ! key)) keys

-- Performs a nested lookup on the Bot's strings JSON.
-- The toResult function converts the inner-most Value to the desired type.
lookUpWith :: MonadBot m => (Value -> a) -> [Text] -> m a
lookUpWith toResult keyNames = do
    strs <- getStrings
    let obj = view Aeson._Object strs
    pure $ toResult (nestedLookup obj keyNames)

lookUpStr :: MonadBot m => [Text] -> m Text
lookUpStr = lookUpWith (^. Aeson._String)

lookUpStrs :: MonadBot m => [Text] -> m [Text]
lookUpStrs = lookUpWith (^.. Aeson.values . Aeson._String)
