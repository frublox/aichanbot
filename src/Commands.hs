{-# LANGUAGE OverloadedStrings #-}

module Commands where

import           Data.Aeson
import           Data.Aeson.Lens
import           Data.HashMap.Strict    (HashMap, (!))
import qualified Data.HashMap.Strict    as HashMap
import           Data.List              (intersperse)
import           Data.Monoid            ((<>))
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Data.Vector            (indexM)

import           Control.Lens
import           Control.Monad.IO.Class (liftIO)

import           System.Random

import           Bot
import           Types
import           Util                   (stripAt)

cmdCommands :: CommandInfo -> Command
cmdCommands = makeCommand $ \source _ -> do
    cmdsStatic <- views commands (map $ view $ info.name)
    cmdsDynamic <- uses dynamicCmds HashMap.keys
    let cmds = map (Text.cons '!') (cmdsStatic <> cmdsDynamic)
    replyTo source (Text.concat $ intersperse ", " cmds)

cmdHi :: CommandInfo -> Command
cmdHi = makeCommand $ \source args -> do
    msg <- view (botData . key "strings" . key "hi" . _String)

    case args of
        (target:_) -> replyTo (stripAt target) msg
        _          -> replyTo source msg

cmdBye :: CommandInfo -> Command
cmdBye = makeCommand $ \source args -> do
    msg <- view (botData . key "strings" . key "bye" . _String)

    case args of
        (target:_) -> replyTo (stripAt target) msg
        _          -> replyTo source msg

cmdAliases :: CommandInfo -> Command
cmdAliases = makeCommand $ \source args ->
    case args of
        (cmdName:_) -> do
            maybeCmd <- lookupCommand cmdName
            case maybeCmd of
                Nothing -> replyTo source ("Couldn't find command !" <> cmdName)
                Just cmd ->
                    replyTo source . Text.concat . intersperse "," . map (Text.cons '!') $
                        (cmd^.info.aliases)
        _ -> replyHelpStr source "aliases"

cmdHelp :: CommandInfo -> Command
cmdHelp = makeCommand $ \source args -> case args of
    (cmdName:_) -> do
        maybeCmd <- lookupCommand cmdName
        case maybeCmd of
            Nothing  -> replyTo source ("Couldn't find command !" <> cmdName)
            Just cmd -> replyTo source (cmd^.info.help)
    _ -> replyHelpStr source "help"

cmdAdd :: CommandInfo -> Command
cmdAdd = makeCommand $ \source args ->
    case args of
        (cmdName:cmdText:_) -> do
            dynamicCmds %= HashMap.insert cmdName cmdText
            msg <- view (botData . key "strings" . key "add" . _String)
            replyTo source (msg <> cmdName)

            saveDynCmds
        _ -> replyHelpStr source "add"

cmdRemove :: CommandInfo -> Command
cmdRemove = makeCommand $ \source args ->
    case args of
        (cmdName:_) -> do
            dynamicCmds %= HashMap.delete cmdName
            msg <- view (botData . key "strings" . key "remove" . _String)
            replyTo source (msg <> cmdName)

            saveDynCmds
        _ -> replyHelpStr source "remove"

cmd8ball :: CommandInfo -> Command
cmd8ball = makeCommand $ \source args ->
    case args of
        [] -> do
            msg <- view (botData . key "strings" . key "8ball" . key "no_args" . _String)
            replyTo source msg
        _  -> do
            rs <- views (botData . key "strings" . key "8ball" . key "responses") (^.. values . _String)

            i <- liftIO $ randomRIO (0, length rs - 1)
            replyTo source (rs !! i)

cmdDynamic :: Text -> Command
cmdDynamic cmdName = makeCommand action (CommandInfo cmdName [] PermAnyone helpStr)
    where
        helpStr = "Usage: !" <> cmdName <> " [optional username]"

        action source args = do
            msg <- uses dynamicCmds (! cmdName)
            case args of
                (target:_) -> replyTo (stripAt target) msg
                _          -> replyTo source msg

staticCommands :: HashMap Text (CommandInfo -> Command)
staticCommands = HashMap.fromList
    [ ("commands", cmdCommands)
    , ("hi", cmdHi)
    , ("bye", cmdBye)
    , ("aliases", cmdAliases)
    , ("help", cmdHelp)
    , ("add", cmdAdd)
    , ("remove", cmdRemove)
    , ("8ball", cmd8ball)
    ]
