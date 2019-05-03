{-# LANGUAGE OverloadedStrings #-}

module Command.Run
    ( runCommand
    ) where

import           Control.Lens
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.HashMap.Strict    (HashMap, (!))
import qualified Data.HashMap.Strict    as HashMap
import           Data.List              (intersperse)
import           Data.Monoid            ((<>))
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Data.Vector            (indexM)
import           System.Random
import           Text.Megaparsec        (parseMaybe)

import           Bot.Monad
import qualified Bot.Util               as Bot
import           Command.Info           (CommandInfo (..), aliases, help, name)
import           Command.Parser         (commandP)
import qualified Command.Permissions    as Perms
import           Command.Type           (Command)
import qualified Command.Type           as Cmd
import           Util                   (stripAt)

runCommand :: MonadBot m => Command -> Text -> [Text] -> m ()
runCommand Cmd.Cmds src _ = do
    cmdNames <- getCmds >>= mapM getCmdInfo >>= pure . fmap (view name)
    Bot.replyTo src (Text.concat . intersperse ", " . map (Text.cons '!') $ cmdNames)

runCommand Cmd.Hi src args = do
    msg <- getStrings >>= pure . view (key "cmds" . key "hi" . _String)

    case args of
        (target:_) -> Bot.replyTo (stripAt target) msg
        _          -> Bot.replyTo src msg

runCommand Cmd.Bye src args = do
    msg <- getStrings >>= pure . view (key "cmds" . key "bye" . _String)

    case args of
        (target:_) -> Bot.replyTo (stripAt target) msg
        _          -> Bot.replyTo src msg

runCommand Cmd.Aliases src args = do
    strs <- getStrings
    let strUnknownCmd = view (key "cmds" . key "aliases" . key "unknownCmd" . _String) strs
    let strNone = view (key "cmds" . key "aliases" . key "none" . _String) strs

    case args of
        (cmdName:_) -> do
            let maybeCmd = parseMaybe commandP cmdName
            case maybeCmd of
                Nothing -> Bot.replyTo src strUnknownCmd
                Just cmd -> do
                    info <- getCmdInfo cmd
                    let toText = Text.concat . intersperse "," . map (Text.cons '!')
                    if null (info^.aliases)
                        then Bot.replyTo src strNone
                        else Bot.replyTo src (toText (info^.aliases))
        _ -> Bot.replyHelpStr src Cmd.Aliases

runCommand Cmd.Help src args = do
    strUnknownCmd <- getStrings
        >>= pure . view (key "cmds" . key "help" . key "unknownCmd" . _String)
    case args of
        (cmdName:_) -> do
            let maybeCmd = parseMaybe commandP cmdName
            case maybeCmd of
                Nothing  -> Bot.replyTo src strUnknownCmd
                Just cmd -> do
                    info <- getCmdInfo cmd
                    Bot.replyTo src (info^.help)
        _ -> Bot.replyHelpStr src Cmd.Help

runCommand _ _ _ = pure ()

-- cmdAdd :: MonadBot m => CommandInfo -> Command m
-- cmdAdd = makeCommand $ \source args ->
--     case args of
--         (cmdName:cmdText:_) -> do
--             addDynCmd cmdName cmdText

--             strs <- getStrings
--             let msg = view (key "strings" . key "add" . _String) strs

--             replyTo source (msg <> cmdName)

--             saveDynCmds
--         _ -> replyHelpStr source "add"

-- cmdRemove :: MonadBot m => CommandInfo -> Command m
-- cmdRemove = makeCommand $ \source args ->
--     case args of
--         (cmdName:_) -> do
--             delDynCmd cmdName
--             msg <- view (botData . key "strings" . key "remove" . _String)
--             replyTo source (msg <> cmdName)

--             saveDynCmds
--         _ -> replyHelpStr source "remove"

-- cmd8ball :: MonadBot m => CommandInfo -> Command m
-- cmd8ball = makeCommand $ \source args ->
--     case args of
--         [] -> do
--             msg <- view (botData . key "strings" . key "8ball" . key "no_args" . _String)
--             replyTo source msg
--         _  -> do
--             rs <- views (botData . key "strings" . key "8ball" . key "responses") (^.. values . _String)

--             i <- liftIO $ randomRIO (0, length rs - 1)
--             replyTo source (rs !! i)

-- cmdDynamic :: MonadBot m => Text -> Command m
-- cmdDynamic cmdName = makeCommand action (CommandInfo cmdName [] PermAnyone helpStr)
--     where
--         helpStr = "Usage: !" <> cmdName <> " [optional username]"

--         action source args = do
--             msg <- uses dynamicCmds (! cmdName)
--             case args of
--                 (target:_) -> replyTo (stripAt target) msg
--                 _          -> replyTo source msg
