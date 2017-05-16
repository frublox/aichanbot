{-# LANGUAGE OverloadedStrings #-}

module Command.Commands where

import           Control.Lens

import           Bot.Actions

import           Command.Types

cmdCommands :: Command
cmdCommands = makeCommand "commands" ["cmds", "list"] PermAnyone $ \source _ -> do
    cmdsStatic <- views (botData . commands) Map.keys
    cmdsDynamic <- uses dynamicCmds Map.keys
    let cmds = map (Text.cons '!') (cmdsStatic <> cmdsDynamic)
    replyTo source (Text.concat $ intersperse ", " cmds)

cmdHi :: Command
cmdHi = makeCommand "hi" [] PermAnyone $ \source args -> do
    msg <- views (botData . strings) (! "hi")

    case args of
        (target:_) -> replyTo target msg
        _          -> replyTo source msg

cmdBye :: Command
cmdBye = makeCommand "bye" [] PermAnyone $ \source args -> do
    msg <- views (botData . strings) (! "bye")

    case args of
        (target:_) -> replyTo target msg
        _          -> replyTo source msg

cmdAliases :: Command
cmdAliases = makeCommand "aliases" [] PermAnyone $ \source args ->
    case args of
        (cmdName:_) -> do
            maybeCmd <- views (botData . commands) (find (\cmd -> cmd^.info.name == cmdName))
            case maybeCmd of
                Nothing -> replyTo source ("Couldn't find command !" <> cmdName)
                Just cmd ->
                    replyTo source . Text.concat . intersperse "," . map (Text.cons '!') $
                        (cmdInfo^.aliases)
        _ -> replyTo source "Invali"

cmdHelp :: Command
cmdHelp = makeCommand "help" [] PermAnyone $ \source args -> do
    maybeCmd <- views (botData . commands) (find (\cmd -> cmd^.info.name == cmdName))
    case maybeCmd of
        Nothing  -> replyTo source ("Couldn't find command !" <> cmdName)
        Just cmd -> replyTo source (cmd^.cmdInfo.help)

cmdAdd :: Command
cmdAdd = makeCommand "add" [] PermModOnly $ \source args ->
    case args of
        (cmdName:cmdText:_) -> do
            dynamicCmds %= Map.insert cmdName cmdText
            msg <- views (botData . strings) (! "add")
            replyTo source (msg <> cmdName)

            saveDynCmds
        _ -> do

            replyTo source "Usage: !add <cmd name> <cmd text>"

cmdRemove :: Command
cmdRemove = makeCommand "remove" [] PermModOnly $ \source args ->
    case args of
        (cmdName:_) -> do
            dynamicCmds %= Map.delete cmdName
            msg <- views (botData . strings) (! "remove")
            replyTo source (msg <> cmdName)

            saveDynCmds
        _ -> replyTo source "Usage: !remove <cmd name>"

cmdDynamic :: Text -> Command
cmdDynamic cmdName = makeCommand cmdName [] PermAnyone $ \source args -> do
    msg <- uses dynamicCmds (! cmdName)
    replyTo source msg
