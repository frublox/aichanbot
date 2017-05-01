{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Ini
import           Data.List                 (intersperse)
import           Data.Monoid               ((<>))
import           Data.Text                 (Text)
import qualified Data.Text                 as Text

import           Control.Concurrent.Lifted (fork)
import           Control.Lens
import           Control.Monad             (when)
import           Control.Monad.IO.Class    (MonadIO, liftIO)

import           Text.Parsec

import           Network.IRC.Client
import           System.Exit               (die)

import           Types
import           Wrappers

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
    botState <- initBotState botConf

    conn <- connectWithTLS' stdoutLogger "irc.chat.twitch.tv" 443 2
    let conn' = conn { _onconnect = onConnect }

    startStateful conn' ircConf botState

    where
        ircConf = defaultIRCConf (botConf^.botNick)
            & password .~ Just (botConf^.pass)
            & eventHandlers .~ handler : defaultEventHandlers

onConnect :: Bot ()
onConnect = do
    s <- state

    let nick = s^.config.botNick
    let chan = s^.config.channel

    send $ RawMsg ("NICK " <> nick)
    send $ RawMsg ("JOIN " <> chan)

    return ()

handler :: EventHandler BotState
handler = EventHandler "bot" EPrivmsg $ \event ->
    case event^.message of
        Privmsg _ (Right msg) -> do
            let parsedCommand = parse command "" msg
            let user = extractUser event

            mapM_ (handleCommand user) parsedCommand

            when ("KonCha" `isSubStrOf` msg) $
                replyTo user "L-lewd!"

        _ -> return ()

    where
        extractUser :: Event Text -> Maybe Text
        extractUser event =
            case event^.source of
                User user      -> Just user
                Channel _ user -> Just user
                _              -> Nothing

        isSubStrOf :: Text -> Text -> Bool
        isSubStrOf x y =
            case Text.breakOn x y of
                (_, "") -> False
                _       -> True

handleCommand :: Maybe Text -> Command -> Bot ()
handleCommand user cmd =
    case cmd of
        CmdUnknown -> replyTo user "idk that command :/"
        CmdHi target -> case target of
            Just _  -> replyTo target "hi! KonCha"
            Nothing -> replyTo user "hi! KonCha"
        CmdBye target -> case target of
            Just _  -> replyTo target "cya! KonCha"
            Nothing -> replyTo user "cya! KonCha"
        CmdCommands -> do
            let commands = Text.concat (intersperse ", " commandList)
            replyTo user ("[arg] means an optional argument, usernames"
                <> " can be with or without an @ symbol --- " <> commands)
