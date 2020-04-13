{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Bot.EventHandlers
    ( pingHandler
    , msgHandler
    )
where

import           Control.Applicative  (liftA2)
import           Control.Lens
import           Control.Monad        (forM, forM_, when)
import           Control.Monad.Logger (MonadLogger, logDebugSH, logErrorSH)
import qualified Data.Aeson.Lens      as Aeson.Lens
import           Data.HashMap.Strict  ((!))
import qualified Data.HashMap.Strict  as HashMap
import           Data.Text            (Text)
import qualified Data.Text            as Text
import qualified Text.Megaparsec      as Megaparsec

import           Bot.Command          (runCommand)
import           Bot.Monad            (MonadBot)
import qualified Bot.Monad            as Bot
import           Bot.Source           (Source)
import qualified Bot.Util             as Bot
import qualified Command.Info         as Info
import           Command.Parser       (argsP, commandP)
import           Command.Permissions  as Perms
import           Irc.Event            (Event (..))
import           Irc.EventHandler     (EventHandler (..))
import           Irc.Parser           (msgSourceP, msgTextP, twitchTagsP)
import           Random.Monad         (MonadRandom, randomR)
import           Util                 (textContains)

pingHandler :: Monad m => EventHandler m
pingHandler =
    EventHandler EPing $ \msg -> pure ["PONG :" <> Text.drop 6 msg]

msgHandler :: (MonadLogger m, MonadRandom m, MonadBot m) => EventHandler m
msgHandler = EventHandler EPrivMsg $ \msg -> do
    let parsedSource = Megaparsec.runParser msgSourceP "" msg
    let parsedMsg = Megaparsec.runParser msgTextP "" msg
    case liftA2 (,) parsedSource parsedMsg of
        Left err -> $(logErrorSH) (Megaparsec.errorBundlePretty err) >> pure []
        Right (src, msgText) -> do
            -- Handle keyphrases
            keyphraseMsgs <- do
                responseKeys <- fmap HashMap.keys (Bot.lookUpObj ["responses"])
                fmap concat $ forM responseKeys $ \key ->
                    if msgText `textContains` key
                        then Bot.lookUpStr ["responses", key] >>= Bot.replyTo src
                        else pure []
            -- Handle commands
            cmdMsgs <- do
                parsedCmd <- Megaparsec.runParserT (commandP Bot.resolveCmd) "" msgText
                let parsedArgs = Megaparsec.runParser argsP "" msgText
                case liftA2 (,) parsedCmd parsedArgs of
                    Left err -> pure []
                    Right (cmd, args) -> do
                        info <- Bot.getCmdInfo cmd
                        let requiredPerms = view Info.permissions info
                        if Perms.fromMsg msg >= requiredPerms
                            then runCommand cmd src args
                            else pure []
            pure (keyphraseMsgs <> cmdMsgs)
