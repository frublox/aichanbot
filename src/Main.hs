{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.List (intersperse)
import Data.Ini
import Data.Monoid ((<>))

import Control.Lens
import Control.Concurrent.Lifted (fork)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Text.Parsec

import Network.IRC.Client hiding (send)
import System.Exit (die)

import Types
import Wrappers

commandList :: [Text]
commandList =
    [ "!hi [username]"
    , "!bye [username]"
    , "!commands (!list, !cmds)"
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

    return (BotConfig nick pass channel)

run :: MonadIO io => BotConfig -> io ()
run botConf = do
    botState <- initBotState

    conn <- connectWithTLS' stdoutLogger "irc.chat.twitch.tv" 443 1

    let conn' = conn { _onconnect = onConnect }
    let env = IrcEnv botConf botState

    newIRCState conn' ircConf env
    startStateful conn' ircConf env

    where
        ircConf = defaultIRCConf (botConf^.botNick)
            & nick .~ (botConf^.botNick)
            & password .~ Just (botConf^.pass) 
            & eventHandlers .~ handler : defaultEventHandlers

onConnect :: StatefulIRC IrcEnv ()
onConnect = runBot $ do
    chan <- use (config.channel)

    send $ RawMsg ("JOIN " <> chan)

    fork rateLimitTimer
    return ()

handler :: EventHandler IrcEnv
handler = EventHandler "bot" EPrivmsg $ \event -> runBot $ do
    case event^.message of
        Privmsg _ (Right msg) -> do
            let parsedCommand = parse command "" msg
            let user = extractUser event

            case parsedCommand of
                Right cmd -> handleCommand user cmd
                _ -> return ()
        _ -> return ()

    where
        extractUser :: Event Text -> Maybe Text
        extractUser event =
            case event^.source of
                User user -> Just user
                Channel _ user -> Just user
                _ -> Nothing

handleCommand :: Maybe Text -> Command -> Bot ()
handleCommand user cmd =
    case cmd of
        CmdUnknown -> replyTo user "idk that command :/"
        CmdHi target -> case target of
            Just _ -> replyTo target "hi! VoHiYo"
            Nothing -> replyTo user "hi! VoHiYo"
        CmdBye target -> case target of
            Just _ -> replyTo target "cya! VoHiYo"
            Nothing -> replyTo user "cya! VoHiYo"
        CmdCommands -> do
            let commands = Text.concat (intersperse ", " commandList)
            replyTo user ("[arg] means an optional argument, usernames"
                <> " can be with or without an @ symbol --- " <> commands)