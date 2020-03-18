{-# LANGUAGE OverloadedStrings #-}

module Bot.Command
    ( runCommand
    ) where

import           Control.Lens
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.HashMap.Strict    (HashMap, (!))
import qualified Data.HashMap.Strict    as HashMap
import           Data.List              (intersperse)
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Data.Vector            (indexM)
import           System.Random
import           Text.Megaparsec        (runParserT, try)

import           Bot.Monad              (MonadBot)
import qualified Bot.Monad              as Bot
import qualified Bot.Util               as Bot
import           Command.Info           (CommandInfo (..), aliases, help, name)
import           Command.Parser         (maybePrefixedCommandP)
import qualified Command.Permissions    as Perms
import           Command.Type           (Command)
import qualified Command.Type           as Cmd
import           Util                   (stripAt)

runCommand :: MonadBot m => Command -> Text -> [Text] -> m ()
runCommand Cmd.Cmds src _ = do
    cmdNames <- fmap (view name) <$> (Bot.getCmds >>= mapM Bot.getCmdInfo)
    dynCmdNames <- Bot.getDynCmdNames
    let f = Text.concat . intersperse ", " . map (Text.cons '!')
    Bot.replyTo src $ f (cmdNames <> dynCmdNames)

runCommand Cmd.Hi src args = do
    msg <- view (key "cmds" . key "hi" . _String) <$> Bot.getStrings

    case args of
        (target:_) -> Bot.replyTo (stripAt target) msg
        _          -> Bot.replyTo src msg

runCommand Cmd.Bye src args = do
    msg <- view (key "cmds" . key "bye" . _String) <$> Bot.getStrings

    case args of
        (target:_) -> Bot.replyTo (stripAt target) msg
        _          -> Bot.replyTo src msg

runCommand Cmd.Aliases src args = do
    strs <- Bot.getStrings
    let strUnknownCmd = view (key "cmds" . key "aliases" . key "unknownCmd" . _String) strs
    let strNone = view (key "cmds" . key "aliases" . key "none" . _String) strs

    case args of
        (cmdName:_) -> do
            result <- runParserT (maybePrefixedCommandP Bot.resolveCmd) "" cmdName
            case result of
                Left _ -> Bot.replyTo src strUnknownCmd
                Right cmd -> do
                    info <- Bot.getCmdInfo cmd
                    let toText = Text.concat . intersperse ", " . map (Text.cons '!')
                    if null (info^.aliases)
                        then Bot.replyTo src strNone
                        else Bot.replyTo src (toText (info^.aliases))
        _ -> Bot.replyHelpStr src Cmd.Aliases

runCommand Cmd.Help src args = do
    strUnknownCmd <- view (key "cmds" . key "help" . key "unknownCmd" . _String)
        <$> Bot.getStrings

    case args of
        (cmdName:_) -> do
            result <- runParserT (maybePrefixedCommandP Bot.resolveCmd) "" cmdName
            case result of
                Left _ -> Bot.replyTo src strUnknownCmd
                Right cmd -> do
                    info <- Bot.getCmdInfo cmd
                    Bot.replyTo src (info^.help)
        _ -> Bot.replyHelpStr src Cmd.Help

runCommand Cmd.AddCmd src args =
    case args of
        (cmdName:cmdText:_) -> do
            Bot.addDynCmd cmdName cmdText

            msg <- view (key "cmds" . key "add" . _String) <$> Bot.getStrings
            Bot.replyTo src (msg <> cmdName)

            Bot.saveDynCmds
        _ -> Bot.replyHelpStr src Cmd.AddCmd

runCommand Cmd.RemoveCmd src args =
    case args of
        (cmdName:_) -> do
            Bot.delDynCmd cmdName
            msg <- view (key "cmds" . key "remove" . _String) <$> Bot.getStrings
            Bot.replyTo src (msg <> cmdName)

            Bot.saveDynCmds
        _ -> Bot.replyHelpStr src Cmd.RemoveCmd

runCommand Cmd.EightBall src args =
    case args of
        [] -> do
            msg <- view (key "cmds" . key "8ball" . key "no_args" . _String)
                <$> Bot.getStrings
            Bot.replyTo src msg
        _  -> do
            rs <- views (key "cmds" . key "8ball" . key "responses") (^.. values . _String)
                <$> Bot.getStrings

            i <- Bot.randomR (0, length rs - 1)
            Bot.replyTo src (rs !! i)

runCommand (Cmd.Dynamic txt) src args =
    case args of
        (target:_) -> Bot.replyTo (stripAt target) txt
        _          -> Bot.replyTo src txt