{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.List (intersperse)
import Data.Ini
import Data.Monoid ((<>))

import Control.Lens
import Control.Monad.IO.Class

import Text.Parsec
import Text.Parsec.Text

import Network.IRC.Client
import System.Exit (die)

import Types
import Wrappers

commandList :: [Text]
commandList =
    [ "!hi"
    , "!commands (!list)"
    ]

main :: IO ()
main = do
    ini <- readIniFile "config.ini" >>= either die return
    conf <- either die return (readConfig ini)

    run conf

readConfig :: Ini -> Either String BotConfig
readConfig ini = do
    nick <- lookupValue "config" "nick" ini
    pass <- lookupValue "config" "pass" ini
    channel <- lookupValue "config" "channel" ini

    let conf = BotConfig nick pass channel
    return conf

run :: MonadIO io => BotConfig -> io ()
run conf = do
    conn <- connectWithTLS "irc.chat.twitch.tv" 443 10
    let conn' = conn { _onconnect = onConnect }
    startStateful conn' cfg (initBotState conf)

    where
        cfg = defaultIRCConf (conf^.botNick) 
            & password .~ Just (conf^.pass) 
            & eventHandlers .~ handler : defaultEventHandlers

onConnect :: StatefulIRC BotState ()
onConnect = do
    s <- state
    send $ RawMsg ("NICK " <> s^.config.botNick)
    send $ RawMsg ("JOIN " <> s^.config.channel)

handler :: EventHandler BotState
handler = EventHandler "bot" EPrivmsg $ \event -> do
    s <- state

    case event^.message of
        Privmsg from (Right msg) -> do
            let parsedCommand = parse command "" msg
            let user = extractUser event

            case parsedCommand of
                Left _ -> return ()
                Right cmd -> handleCommand user cmd
        _ -> return ()

    where
        extractUser :: Event Text -> Maybe Text
        extractUser event =
            case event^.source of
                User user -> Just user
                Channel _ user -> Just user
                _ -> Nothing

handleCommand :: Maybe Text -> Command -> StatefulIRC BotState ()
handleCommand user cmd =
    case cmd of
        CmdUnknown -> replyTo user "idk that command :/"
        CmdHi -> replyTo user "hi! VoHiYo"
        CmdCommands -> do
            let msg = Text.concat (intersperse ", " commandList)
            replyTo user msg