{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Aeson             (eitherDecode)
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy   as BytesL
import           Data.HashMap.Strict    ((!))
import qualified Data.HashMap.Strict    as HashMap
import           Data.Ini
import           Data.Monoid            ((<>))

import           Control.Applicative    (liftA2)
import           Control.Lens
import           Control.Monad          (forM_, when)
import           Control.Monad.IO.Class (MonadIO, liftIO)

import           System.Exit            (die)

import           Text.Megaparsec        (parse, runParserT)

import           Bot
import           Command.Parser         (invocation)
import           Commands               (staticCommands)
import           Irc
import           Types
import           Util                   (textContains)

main :: IO ()
main = do
    ini <- readIniFile "config.ini" >>= either die return
    cmds <- loadCommands
    botConfig <- initBotConfig cmds ini >>= either die return

    let handlers = [pingHandler, msgHandler]
    let ircBot = IrcBot botSetup handlers botConfig initBotState

    runIrcBot 6667 "irc.chat.twitch.tv" ircBot

botSetup :: Bot ()
botSetup = do
    pass <- view botPass
    nick <- view botNick
    chan <- view channel

    send "CAP REQ :twitch.tv/tags"
    send ("PASS :" <> pass)
    send ("NICK :" <> nick)
    send ("JOIN :" <> chan)

    cmds <- readDynCmds
    dynamicCmds .= cmds

msgHandler :: EventHandler
msgHandler = EventHandler EPrivMsg $ \ircMsg -> do
    let parsedSource = parse ircMsgSource "" ircMsg
    let parsedText = parse ircMsgText "" ircMsg

    forM_ (liftA2 (,) parsedSource parsedText) $ \(source, text) -> do
        -- reply appropriately when text contains a keyphrase
        responseKeys <- views (botData . key "responses" . _Object) HashMap.keys
        forM_ responseKeys $ \k ->
            when (text `textContains` k) $ do
                response <- view (botData . key "responses" . key k . _String)
                replyTo source response

        perms <- getPermissions ircMsg

        invoc <- runParserT (invocation source) "" text

        forM_ invoc $ \invoc' ->
            when (perms >= invoc'^.invocOf.info.permissions) $
                invoc'^.runResult

loadCommands :: MonadIO io => io [Command]
loadCommands = liftIO $ do
    bytes <- BytesL.readFile "cmds.json"
    case eitherDecode bytes of
        Left err       -> die err
        Right cmdInfos -> return $
            map (\info' -> staticCommands ! view name info' $ info') cmdInfos
