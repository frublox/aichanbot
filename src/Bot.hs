{-# LANGUAGE OverloadedStrings #-}

module Bot where

import           Data.Aeson
import qualified Data.ByteString.Lazy   as BytesL
import           Data.List              (find)
import           Data.Map.Strict        (Map, (!))
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
import           Util                   (textContains)

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

readDynCmds :: Bot (Map Text Text)
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
    return $ find
        (\cmd -> cmdName == cmd^.info.name || cmdName `elem` cmd^.info.aliases)
        cmds

getCommand :: Text -> Bot Command
getCommand cmdName = do
    cmds <- view commands
    let cmd = find
            (\c -> cmdName == c^.info.name || cmdName `elem` c^.info.aliases)
            cmds
    maybe (fail $ "Couldn't find command " <> unpack cmdName) return cmd

replyHelpStr :: Text -> Text -> Bot ()
replyHelpStr source cmdName = do
    cmd <- getCommand cmdName
    replyTo source (cmd^.info.help)

