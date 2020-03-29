{-# LANGUAGE OverloadedStrings #-}

module Bot.Command
    ( runCommand
    )
where

import           Control.Lens
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.HashMap.Strict            ( HashMap
                                                , (!)
                                                )
import qualified Data.HashMap.Strict           as HashMap
import           Data.List                      ( intersperse )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Vector                    ( indexM )
import           Text.Megaparsec                ( runParserT
                                                , try
                                                )
import           Random.Monad                   ( MonadRandom
                                                , randomR
                                                )
import           Bot.Source                     ( Source )
import qualified Bot.Source                    as Source
import           Bot.Monad                      ( MonadBot )
import qualified Bot.Monad                     as Bot
import qualified Bot.Util                      as Bot
import           Command.Info                   ( CommandInfo(..)
                                                , aliases
                                                , help
                                                , name
                                                )
import           Command.Parser                 ( maybePrefixedCommandP )
import qualified Command.Permissions           as Perms
import           Command.Type                   ( Command )
import qualified Command.Type                  as Cmd

runCommand :: (MonadRandom m, MonadBot m) => Command -> Source -> [Text] -> m ()
runCommand Cmd.Cmds src _ = do
    cmdInfos <- Bot.getCmds >>= mapM Bot.getCmdInfo
    let cmdNames = fmap (view name) cmdInfos
    dynCmdNames <- Bot.getDynCmdNames
    let toCmdList = Text.concat . intersperse ", " . map (Text.cons '!')
    Bot.replyTo src $ toCmdList (cmdNames <> dynCmdNames)

runCommand Cmd.Hi src args = do
    reply <- Bot.lookUpStr ["cmds", "hi"]
    replyCmd src args reply

runCommand Cmd.Bye src args = do
    reply <- Bot.lookUpStr ["cmds", "bye"]
    replyCmd src args reply

runCommand Cmd.Aliases src args = do
    strUnknownCmd <- Bot.lookUpStr ["cmds", "aliases", "unknownCmd"]
    strNone       <- Bot.lookUpStr ["cmds", "aliases", "none"]

    case args of
        (cmdName : _) -> do
            result <- runParserT (maybePrefixedCommandP Bot.resolveCmd)
                                 ""
                                 cmdName
            case result of
                Left  _   -> Bot.replyTo src strUnknownCmd
                Right cmd -> do
                    info <- Bot.getCmdInfo cmd
                    let response =
                            Text.concat
                                .  intersperse ", "
                                .  map (Text.cons '!')
                                $  info
                                ^. aliases
                    if Text.null response
                        then Bot.replyTo src strNone
                        else Bot.replyTo src response
        _ -> Bot.replyHelpStr src Cmd.Aliases

runCommand Cmd.Help src args = do
    strUnknownCmd <- Bot.lookUpStr ["cmds", "help", "unknownCmd"]

    case args of
        (cmdName : _) -> do
            result <- runParserT (maybePrefixedCommandP Bot.resolveCmd)
                                 ""
                                 cmdName
            case result of
                Left  _   -> Bot.replyTo src strUnknownCmd
                Right cmd -> do
                    info <- Bot.getCmdInfo cmd
                    Bot.replyTo src (info ^. help)
        _ -> Bot.replyHelpStr src Cmd.Help

runCommand Cmd.AddCmd src args = case args of
    (cmdName : cmdText : _) -> do
        Bot.addDynCmd cmdName cmdText

        msg <- Bot.lookUpStr ["cmds", "add"]
        Bot.replyTo src (msg <> cmdName)

        Bot.saveDynCmds
    _ -> Bot.replyHelpStr src Cmd.AddCmd

runCommand Cmd.RemoveCmd src args = case args of
    (cmdName : _) -> do
        Bot.delDynCmd cmdName

        msg <- Bot.lookUpStr ["cmds", "remove"]
        Bot.replyTo src (msg <> cmdName)

        Bot.saveDynCmds
    _ -> Bot.replyHelpStr src Cmd.RemoveCmd

runCommand Cmd.EightBall src args = case args of
    [] -> do
        msg <- Bot.lookUpStr ["cmds", "8ball", "no_args"]
        Bot.replyTo src msg
    _ -> do
        rs <- Bot.lookUpStrs ["cmds", "8ball", "responses"]
        i <- randomR (0, length rs - 1)
        Bot.replyTo src (rs !! i)

runCommand (Cmd.Dynamic txt) src args = case args of
    (target : _) -> Bot.replyTo (Source.fromText target) txt
    _            -> Bot.replyTo src txt

-- Reply to the source with a particular string.
replyCmd :: (MonadRandom m, MonadBot m) => Source -> [Text] -> Text -> m ()
replyCmd src args reply = case args of
    (target : _) -> Bot.replyTo (Source.fromText target) reply
    _            -> Bot.replyTo src reply
