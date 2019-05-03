{-# LANGUAGE OverloadedStrings #-}

module Bot.EventHandlers
    ( pingHandler
    , msgHandler
    ) where

import           Control.Applicative (liftA2)
import           Control.Lens
import           Control.Monad       (forM_, when)
import           Data.Aeson.Lens     (key, _Object, _String)
import qualified Data.HashMap.Strict as HashMap
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           Text.Megaparsec     (parseMaybe)

import           Bot.Monad           (MonadBot)
import qualified Bot.Monad           as Bot
import qualified Bot.Util            as Bot
import qualified Command.Info        as Info
import           Command.Parser      (commandWithArgsP)
import           Command.Permissions as Perms
import           Command.Run         (runCommand)
import           Irc.Event           (Event (..))
import           Irc.EventHandler    (EventHandler (..))
import           Irc.Parser          (msgSourceP, msgTextP)
import           Util                (textContains)

pingHandler :: MonadBot m => EventHandler m
pingHandler = EventHandler EPing $ \msg ->
    Bot.sendMsg ("PONG :" <> Text.drop 6 msg)

msgHandler ::  MonadBot m => EventHandler m
msgHandler = EventHandler EPrivMsg $ \msg -> do
    let parsedSource = parseMaybe msgSourceP msg
    let parsedText = parseMaybe msgTextP msg

    forM_ (liftA2 (,) parsedSource parsedText) $ \(src, text) -> do
        handleKeyphrase src text
        let srcPerms = Perms.fromText msg
        handleCmd src srcPerms text

    where
        handleKeyphrase :: MonadBot m => Text -> Text -> m ()
        handleKeyphrase src text = do
            strs <- Bot.getStrings

            let responseKeys = views (key "responses" . _Object) HashMap.keys strs
            forM_ responseKeys $ \k ->
                when (text `textContains` k) $ do
                    let response = view (key "responses" . key k . _String) strs
                    Bot.replyTo src response

        handleCmd :: MonadBot m => Text -> Permissions -> Text -> m ()
        handleCmd src srcPerms text =
            forM_ (parseMaybe commandWithArgsP text) $ \(cmd, args) -> do
                requiredPerms <- view Info.permissions <$> Bot.getCmdInfo cmd
                when (srcPerms >= requiredPerms) $
                    runCommand cmd src args
