{-# LANGUAGE OverloadedStrings #-}

module Bot.Command
    ( runCommand
    )
where

import           Bot.Monad              (MonadBot)
import qualified Bot.Monad              as Bot
import           Bot.Source             (Source)
import qualified Bot.Source             as Source
import qualified Bot.Util               
import           Command.Info           (CommandInfo (..), aliases, help, name)
import           Command.Parser         (maybePrefixedCommandP)
import qualified Command.Permissions    as Perms
import           Command.Type           (Command)
import qualified Command.Type           as Cmd
import           Control.Lens
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Logger   (MonadLogger, logDebugSH)
import           Data.HashMap.Strict    (HashMap, (!))
import qualified Data.HashMap.Strict    as HashMap
import           Data.List              (intersperse)
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Data.Vector            (indexM)
import           Random.Monad           (MonadRandom, randomR)
import           Text.Megaparsec        (runParserT, try)

runCommand :: (MonadLogger m, MonadRandom m, MonadBot m)
    => Command -> Source -> [Text] -> m [Text]
runCommand Cmd.Cmds src _ = do
    cmdInfos <- Bot.getCmds >>= mapM Bot.getCmdInfo
    let cmdNames = fmap (view name) cmdInfos
    dynCmdNames <- Bot.getDynCmdNames
    let toCmdList = Text.concat . intersperse ", " . map (Text.cons '!')
    Bot.Util.replyTo src $ toCmdList (cmdNames <> dynCmdNames)

runCommand Cmd.Hi src args = do
    reply <- Bot.Util.lookUpStr ["cmds", "hi"]
    replyCmd src args reply

runCommand Cmd.Bye src args = do
    reply <- Bot.Util.lookUpStr ["cmds", "bye"]
    replyCmd src args reply

runCommand Cmd.Aliases src args = do
    strUnknownCmd <- Bot.Util.lookUpStr ["cmds", "aliases", "unknownCmd"]
    strNone       <- Bot.Util.lookUpStr ["cmds", "aliases", "none"]
    case args of
        (cmdName : _) -> do
            result <- runParserT (maybePrefixedCommandP Bot.resolveCmd)
                                 ""
                                 cmdName
            case result of
                Left  _   -> Bot.Util.replyTo src strUnknownCmd
                Right cmd -> do
                    info <- Bot.getCmdInfo cmd
                    let response =
                            Text.concat
                                .  intersperse ", "
                                .  map (Text.cons '!')
                                $  info
                                ^. aliases
                    if Text.null response
                        then Bot.Util.replyTo src strNone
                        else Bot.Util.replyTo src response
        _ -> Bot.Util.replyHelpStr src Cmd.Aliases

runCommand Cmd.Help src args = do
    strUnknownCmd <- Bot.Util.lookUpStr ["cmds", "help", "unknownCmd"]
    case args of
        (cmdName : _) -> do
            result <- runParserT (maybePrefixedCommandP Bot.resolveCmd)
                                 ""
                                 cmdName
            case result of
                Left  _   -> Bot.Util.replyTo src strUnknownCmd
                Right cmd -> do
                    info <- Bot.getCmdInfo cmd
                    Bot.Util.replyTo src (info ^. help)
        _ -> Bot.Util.replyHelpStr src Cmd.Help

runCommand Cmd.AddCmd src args = case args of
    (cmdName : cmdText : _) -> do
        Bot.addDynCmd cmdName cmdText
        Bot.saveDynCmds

        msg <- Bot.Util.lookUpStr ["cmds", "add"]
        Bot.Util.replyTo src (msg <> cmdName)
    _ -> Bot.Util.replyHelpStr src Cmd.AddCmd

runCommand Cmd.RemoveCmd src args = case args of
    (cmdName : _) -> do
        Bot.delDynCmd cmdName
        Bot.saveDynCmds

        msg <- Bot.Util.lookUpStr ["cmds", "remove"]
        Bot.Util.replyTo src (msg <> cmdName)
    _ -> Bot.Util.replyHelpStr src Cmd.RemoveCmd

runCommand Cmd.EightBall src args = case args of
    [] -> do
        msg <- Bot.Util.lookUpStr ["cmds", "8ball", "no_args"]
        Bot.Util.replyTo src msg
    _ -> do
        replies <- Bot.Util.lookUpStrs ["cmds", "8ball", "responses"]
        i <- randomR (0, length replies - 1)
        Bot.Util.replyTo src (replies !! i)

runCommand Cmd.Reload src _ = do
    Bot.reload
    msg <- Bot.Util.lookUpStr ["cmds", "reload", "success"]
    Bot.Util.replyTo src msg

runCommand (Cmd.Dynamic msg) src args = case args of
    (target : _) -> Bot.Util.replyTo (Source.fromText target) msg
    _            -> Bot.Util.replyTo src msg

-- Reply to the source with a particular string.
replyCmd :: (MonadRandom m, MonadBot m) => Source -> [Text] -> Text -> m [Text]
replyCmd src args reply = case args of
    (target : _) -> Bot.Util.replyTo (Source.fromText target) reply
    _            -> Bot.Util.replyTo src reply
