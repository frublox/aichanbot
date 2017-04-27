{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Either.Combinators
import           Data.Ini
import           Data.List                 (intersperse)
import qualified Data.Map.Strict           as Map
import           Data.Monoid               ((<>))
import           Data.Text                 (Text)
import qualified Data.Text                 as Text

import           Control.Concurrent.Lifted (fork)
import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Control.Monad.Trans.Maybe

import           Text.Megaparsec

import           Network.IRC.Client
import           System.Exit               (die)

import           Lifted
import           Types
import           Wrappers

commands :: [Text]
commands =
    [ "!hi"
    , "!bye"
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
    chan <- lookupValue "config" "channel" ini

    return (BotConfig nick pass chan)

run :: MonadIO io => BotConfig -> io ()
run botConf = do
    botState <- initBotState botConf

    conn <- connectWithTLS' stdoutLogger "irc.chat.twitch.tv" 443 2
    let conn' = conn { _onconnect = onConnect }

    startStateful conn' ircConf botState

    where
        ircConf = defaultIRCConf (botConf^.botNick)
            & password .~ Just (botConf^.pass)
            & eventHandlers .~ msgHandler : defaultEventHandlers

onConnect :: Bot ()
onConnect = do
    s <- state

    let nickname = s^.config.botNick
    let chan = s^.config.channel

    send $ RawMsg ("NICK " <> nickname)
    send $ RawMsg ("JOIN " <> chan)

    return ()

msgHandler :: EventHandler BotState
msgHandler = EventHandler "bot" EPrivmsg $ \event ->
    case event^.message of
        Privmsg _ (Right msg) -> do
            let user = extractUser event
            cmd <- runParserT command "" msg

            mapM_ (handleCommand user) cmd

        _ -> return ()

    where
        extractUser :: Event Text -> Maybe Text
        extractUser event =
            case event^.source of
                User user      -> Just user
                Channel _ user -> Just user
                _              -> Nothing

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
            dynCmds <- fmap (map (cons '!')) getDynCommands
            let cmdList = (Text.concat . intersperse ", ") (commands <> dynCmds)
            replyTo user cmdList

        CmdAdd cmdName cmdText -> do
            addCommand cmdName cmdText
            replyTo user ("Added command !" <> cmdName)
        CmdRemove cmdName -> do
            removeCommand cmdName
            replyTo user ("Removed command !" <> cmdName)
        CmdDynamic cmdName -> runDynCommand cmdName

addCommand :: Text -> Text -> Bot ()
addCommand cmdName cmdText = do
    s <- state
    atomicallyL $ modifyTVar (s^.dynamicCmds) (Map.insert cmdName cmdText)

removeCommand :: Text -> Bot ()
removeCommand cmdName = do
    s <- state
    atomicallyL $ modifyTVar (s^.dynamicCmds) (Map.delete cmdName)

runDynCommand :: Text -> Bot ()
runDynCommand cmdName = do
    s <- state
    cmdMap <- readTVarIOL (s^.dynamicCmds)

    mapM_ announce (Map.lookup cmdName cmdMap)

getDynCommands :: Bot [Text]
getDynCommands = do
    s <- state
    cmdMap <- readTVarIOL (s^.dynamicCmds)

    return (Map.keys cmdMap)
