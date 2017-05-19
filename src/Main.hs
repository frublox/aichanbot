{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Aeson             (eitherDecode)
import qualified Data.ByteString.Lazy   as BytesL
import           Data.Ini
import           Data.Map.Strict        ((!))
import qualified Data.Map.Strict        as Map
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
        responseStrs <- view (botData.responses)
        forM_ (Map.keys responseStrs) $ \key ->
            when (text `textContains` key) $
                replyTo source (responseStrs ! key)

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
