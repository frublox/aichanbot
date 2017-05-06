{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Ini
import           Data.List       (intersperse)
import           Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map
import           Data.Monoid     ((<>))
import           Data.Text       (Text)
import qualified Data.Text       as Text

import           Control.Lens
import           Control.Monad   (forM_, when)

import           System.Exit     (die)

import           Text.Megaparsec (parse, runParserT)

import           Bot
import           Command
import           Irc
import           Util            (textContains)

main :: IO ()
main = do
    ini <- readIniFile "config.ini" >>= either die return
    botConfig <- initBotConfig ini >>= either die return

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

    case (,) <$> parsedSource <*> parsedText of
        Right (source, text) -> do
            responseStrs <- view (botData . responses)

            forM_ (Map.keys responseStrs) $ \key ->
                when (text `textContains` key) $
                    replyTo source (responseStrs ! key)

            cmd <- runParserT command "" text
            mapM_ (handleCmd source) cmd
        _ -> return ()

handleCmd :: Text -> Command -> Bot ()
handleCmd source cmd = case cmd of
    CmdHi target  -> do
        msg <- views (botData . strings) (! "hi")
        maybe (replyTo source msg) (`replyTo` msg) target

    CmdBye target -> do
        msg <- views (botData . strings) (! "bye")
        maybe (replyTo source msg) (`replyTo` msg) target

    CmdCommands -> do
        cmdsStatic <- views (botData . commands) Map.keys
        cmdsDynamic <- uses dynamicCmds Map.keys
        let cmds = map (Text.cons '!') (cmdsStatic <> cmdsDynamic)
        replyTo source (Text.concat $ intersperse ", " cmds)

    CmdAdd cmdName cmdText -> do
        dynamicCmds %= Map.insert cmdName cmdText
        msg <- views (botData . strings) (! "add")
        replyTo source (msg <> cmdName)

        saveDynCmds

    CmdRemove cmdName -> do
        dynamicCmds %= Map.delete cmdName
        msg <- views (botData . strings) (! "remove")
        replyTo source (msg <> cmdName)

        saveDynCmds

    CmdDynamic cmdName -> do
        msg <- uses dynamicCmds (! cmdName)
        replyTo source msg

    _             -> return ()
