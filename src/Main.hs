{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Ini
import           Data.List              (intersperse)
import qualified Data.Map.Strict        as Map
import           Data.Monoid            ((<>))
import           Data.Text              (Text)
import qualified Data.Text              as Text

import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad.IO.Class (MonadIO)

import           Text.Megaparsec        (runParserT)

import           Network.IRC.Client
import           System.Exit            (die)

import           Bot
import           Command
import           CommandParser
import           IrcLenses
import           Lifted                 (atomicallyL, readTVarIOL)
import           Wrappers

main :: IO ()
main = do
    ini <- readIniFile "config.ini" >>= either die return
    conf <- either die return (readConfig ini)

    runBot conf

readConfig :: Ini -> Either String BotConfig
readConfig ini =
    BotConfig
        <$> lookupValue "config" "nick" ini
        <*> lookupValue "config" "pass" ini
        <*> lookupValue "config" "channel" ini

runBot :: MonadIO io => BotConfig -> io ()
runBot botConf = do
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
            s <- state

            dynCmds <- fmap (map (cons '!')) getDynCommands
            let commands = map (cons '!') $ Map.keys (s^.staticCmds)
            let cmdList = (Text.concat . intersperse ", ") (commands <> dynCmds)

            replyTo user cmdList

        CmdAdd cmdName cmdText -> do
            s <- state

            cmdAliases <- getAliases
            dynCmds <- getDynCommands

            if Map.member cmdName (s^.staticCmds) || cmdName `elem` cmdAliases || cmdName `elem` dynCmds
                then
                    replyTo user ("!" <> cmdName <> " is already the name of an existing command")
                else do
                    addDynCommand cmdName cmdText
                    replyTo user ("Added command !" <> cmdName)

        CmdRemove cmdName -> do
            removeDynCommand cmdName
            replyTo user ("Removed command !" <> cmdName)
        CmdDynamic cmdName -> runDynCommand cmdName

addDynCommand :: Text -> Text -> Bot ()
addDynCommand cmdName cmdText = do
    s <- state
    atomicallyL $ modifyTVar (s^.dynamicCmds) (Map.insert cmdName cmdText)

removeDynCommand :: Text -> Bot ()
removeDynCommand cmdName = do
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

getAliases :: Bot [Text]
getAliases = do
    s <- state
    return (concat $ (s^.staticCmds) ^.. traverse . aliases)
