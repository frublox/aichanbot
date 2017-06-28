{-# LANGUAGE OverloadedStrings #-}

module Bot where

import           Data.Aeson
import qualified Data.ByteString.Lazy   as BytesL
import           Data.HashMap.Strict    (HashMap, (!))
import qualified Data.HashMap.Strict    as HashMap
import           Data.List              (find)
import           Data.Monoid            ((<>))
import           Data.Text              (Text, unpack)

import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad.IO.Class (liftIO)

import           System.Exit            (die)
import           Text.Megaparsec        (parse, parseErrorPretty)

import           Irc.Parser
import           Lifted                 (atomicallyL)
import           Types
import           Util                   (stripAt, textContains)

send :: Text -> Bot ()
send msg = do
    output <- view outputChan
    atomicallyL (writeTChan output msg)

privMsg :: Text -> Bot ()
privMsg msg = do
    chan <- view channel
    let ircMsg = "PRIVMSG " <> chan <> " :" <> msg
    send ircMsg

replyTo :: Text -> Text -> Bot ()
replyTo user msg = privMsg ("@" <> user <> " " <> msg)

getMsgSource :: Text -> Bot Text
getMsgSource ircMsg = do
    let result = parse ircMsgSource "" ircMsg
    either (liftIO . die . parseErrorPretty) return result

getMsgText :: Text -> Bot Text
getMsgText ircMsg = do
    let result = parse ircMsgText "" ircMsg
    either (liftIO . die . parseErrorPretty) return result

readDynCmds :: Bot (HashMap Text Text)
readDynCmds = liftIO $ do
    bytes <- BytesL.readFile "dynamic_cmds.json"
    either die return (eitherDecode bytes)

saveDynCmds :: Bot ()
saveDynCmds = do
    cmds <- use dynamicCmds
    liftIO $ BytesL.writeFile "dynamic_cmds.json" (encode cmds)

getPermissions :: Text -> Bot CmdPermissions
getPermissions ircMsg = do
    let p = parse twitchTags "" ircMsg

    case p of
        Right tags -> do
            let hasMod = "mod" `elem` (tags ! "user-type")
            let isCaster = any (`textContains` "broadcaster") (tags ! "badges")
            if hasMod || isCaster
                then return PermModOnly
                else return PermAnyone
        _ -> return PermAnyone

lookupCommand :: Text -> Bot (Maybe Command)
lookupCommand cmdName = do
    cmds <- view commands
    let cmd = find
            (\c -> cmdName == c^.info.name || cmdName `elem` c^.info.aliases)
            cmds
    case cmd of
        Nothing -> do
            dynCmds <- uses dynamicCmds HashMap.keys
            return $ cmdDynamic <$> find (== cmdName) dynCmds
        Just _ -> return cmd

    where
        -- Redefined to avoid import cycles
        cmdDynamic :: Text -> Command
        cmdDynamic cmdName' = makeCommand action (CommandInfo cmdName' [] PermAnyone helpStr)
            where
                helpStr = "Usage: !" <> cmdName' <> " [optional username]"

                action source args = do
                    msg <- uses dynamicCmds (! cmdName')
                    case args of
                        (target:_) -> replyTo (stripAt target) msg
                        _          -> replyTo source msg

getCommand :: Text -> Bot Command
getCommand cmdName = do
    cmd <- lookupCommand cmdName
    maybe (fail $ "Couldn't find command " <> unpack cmdName) return cmd

replyHelpStr :: Text -> Text -> Bot ()
replyHelpStr source cmdName = do
    cmd <- getCommand cmdName
    replyTo source (cmd^.info.help)

