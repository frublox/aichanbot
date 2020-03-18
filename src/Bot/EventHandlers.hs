{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Bot.EventHandlers
    ( pingHandler
    , msgHandler
    )
where

import           Control.Applicative            ( liftA2 )
import           Control.Lens
import           Control.Monad                  ( forM_
                                                , when
                                                )
import           Control.Monad.Logger
import           Data.Aeson.Lens                ( key
                                                , _Object
                                                , _String
                                                )
import qualified Data.HashMap.Strict           as HashMap
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Text.Megaparsec                ( errorBundlePretty
                                                , runParser
                                                , runParserT
                                                )

import Bot.Source (Source)
import           Bot.Command                    ( runCommand )
import           Bot.Monad                      ( MonadBot )
import qualified Bot.Monad                     as Bot
import qualified Bot.Util                      as Bot
import qualified Command.Info                  as Info
import           Command.Parser                 ( argsP
                                                , commandP
                                                )
import           Command.Permissions           as Perms
import           Random.Monad                   ( MonadRandom
                                                , randomR
                                                )
import           Irc.Event                      ( Event(..) )
import           Irc.EventHandler               ( EventHandler(..) )
import           Irc.Parser                     ( msgSourceP
                                                , msgTextP
                                                )
import           Util                           ( textContains )

pingHandler :: MonadBot m => EventHandler m
pingHandler =
    EventHandler EPing $ \msg -> Bot.sendMsg ("PONG :" <> Text.drop 6 msg)

msgHandler :: (MonadLogger m, MonadRandom m, MonadBot m) => EventHandler m
msgHandler = EventHandler EPrivMsg $ \msg -> do
    let parsedSource = runParser msgSourceP "" msg
    let parsedText   = runParser msgTextP "" msg

    case liftA2 (,) parsedSource parsedText of
        Left  err         -> $(logErrorSH) (errorBundlePretty err)
        Right (src, text) -> do
            handleKeyphrase src text
            let srcPerms = Perms.fromText msg
            handleCmd src srcPerms text
  where
    -- If the message contains a keyphrase, reply to it with a certain response.
    handleKeyphrase :: (MonadLogger m, MonadBot m) => Source -> Text -> m ()
    handleKeyphrase src text = do
        strs <- Bot.getStrings
        let responseKeys = views (key "responses" . _Object) HashMap.keys strs
        forM_ responseKeys $ \k -> when (text `textContains` k) $ do
            let response = view (key "responses" . key k . _String) strs
            Bot.replyTo src response

    -- Tries to parse a command invocation from the message, and runs the command
    -- if the source has sufficient permissions.
    handleCmd
        :: (MonadLogger m, MonadRandom m, MonadBot m)
        => Source
        -> Permissions
        -> Text
        -> m ()
    handleCmd src srcPerms text = do
        parsedCmd <- runParserT (commandP Bot.resolveCmd) "" text
        let parsedArgs = runParser argsP "" text
        case liftA2 (,) parsedCmd parsedArgs of
            Left  err         -> pure ()
            Right (cmd, args) -> do
                $(logDebugSH) ("Handling cmd: " <> show cmd)
                info <- Bot.getCmdInfo cmd
                let requiredPerms = view Info.permissions info
                when (srcPerms >= requiredPerms) $ 
                    runCommand cmd src args
