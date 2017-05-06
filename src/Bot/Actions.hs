{-# LANGUAGE OverloadedStrings #-}

module Bot.Actions where

import           Data.Aeson
import qualified Data.ByteString.Lazy   as BytesL
import           Data.Map.Strict        (Map)
import           Data.Monoid            ((<>))
import           Data.Text              (Text)

import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad.IO.Class (liftIO)

import           System.Exit            (die)
import           Text.Megaparsec        (parse, parseErrorPretty)

import           Bot.Types
import           Irc.Parser
import           Lifted                 (atomicallyL)

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
