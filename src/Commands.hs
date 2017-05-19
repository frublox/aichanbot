{-# LANGUAGE OverloadedStrings #-}

module Commands where

import           Data.List       (intersperse)
import           Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import           Data.Monoid     ((<>))
import           Data.Text       (Text)
import qualified Data.Text       as Text

import           Control.Lens

import           Bot
import           Types
import           Util            (stripAt)

cmdCommands :: CommandInfo -> Command
cmdCommands = makeCommand $ \source _ -> do
    cmdsStatic <- views commands (map $ view $ info.name)
    cmdsDynamic <- uses dynamicCmds Map.keys
    let cmds = map (Text.cons '!') (cmdsStatic <> cmdsDynamic)
    replyTo source (Text.concat $ intersperse ", " cmds)

cmdHi :: CommandInfo -> Command
cmdHi = makeCommand $ \source args -> do
    msg <- views (botData . strings) (! "hi")

    case args of
        (target:_) -> replyTo (stripAt target) msg
        _          -> replyTo source msg

cmdBye :: CommandInfo -> Command
cmdBye = makeCommand $ \source args -> do
    msg <- views (botData . strings) (! "bye")

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
            dynamicCmds %= Map.insert cmdName cmdText
            msg <- views (botData.strings) (! "add")
            replyTo source (msg <> cmdName)

            saveDynCmds
        _ -> replyHelpStr source "add"

cmdRemove :: CommandInfo -> Command
cmdRemove = makeCommand $ \source args ->
    case args of
        (cmdName:_) -> do
            dynamicCmds %= Map.delete cmdName
            msg <- views (botData . strings) (! "remove")
            replyTo source (msg <> cmdName)

            saveDynCmds
        _ -> replyHelpStr source "remove"

cmdDynamic :: Text -> Command
cmdDynamic cmdName = makeCommand action (CommandInfo cmdName [] PermAnyone helpStr)
    where
        helpStr = "Usage: !" <> cmdName

        action source _ = do
            msg <- uses dynamicCmds (! cmdName)
            replyTo source msg

staticCommands :: Map Text (CommandInfo -> Command)
staticCommands = Map.fromList
    [ ("commands", cmdCommands)
    , ("hi", cmdHi)
    , ("bye", cmdBye)
    , ("aliases", cmdAliases)
    , ("help", cmdHelp)
    , ("add", cmdAdd)
    , ("remove", cmdRemove)
    ]
