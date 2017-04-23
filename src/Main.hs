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

import           Text.Parsec

import           Network.IRC.Client
import           System.Exit               (die)

import           Lifted
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
            & eventHandlers .~ msgHandler : defaultEventHandlers

onConnect :: Bot ()
onConnect = do
    s <- state

    let nick = s^.config.botNick
    let chan = s^.config.channel

    send $ RawMsg ("NICK " <> nick)
    send $ RawMsg ("JOIN " <> chan)

    return ()

msgHandler :: EventHandler BotState
msgHandler = EventHandler "bot" EPrivmsg $ \event -> do
    case event^.message of
        Privmsg _ (Right msg) -> do
            let user = extractUser event
            cmd <- runMaybeT (extractCommand msg)

            case cmd of
                Just cmd' -> handleCommand user cmd'
                Nothing   -> return ()

        _ -> return ()

    where
        extractUser :: Event Text -> Maybe Text
        extractUser event =
            case event^.source of
                User user      -> Just user
                Channel _ user -> Just user
                _              -> Nothing

        extractCommand :: Text -> MaybeT Bot Command
        extractCommand msg = do
            cmdName <- liftMaybe $ rightToMaybe (parse commandName "" msg)
            let dynamicCmd = fmap liftMaybe (lookupDynCmd cmdName)
            let parsedCmd = liftMaybe $ rightToMaybe (parse command "" msg)

            dynamicCmd <|> parsedCmd

handleCommand :: Maybe Text -> Command -> Bot ()
handleCommand user cmd =
    case cmd of
        CmdUnknown -> replyTo user "idk that command :/"

        CmdHi target -> case target of
            Just _  -> replyTo target "hi! VoHiYo"
            Nothing -> replyTo user "hi! VoHiYo"
        CmdBye target -> case target of
            Just _  -> replyTo target "cya! VoHiYo"
            Nothing -> replyTo user "cya! VoHiYo"
        CmdCommands -> do
            let commands = Text.concat (intersperse ", " commandList)
            replyTo user ("[arg] means an optional argument, usernames"
                <> " can be with or without an @ symbol --- " <> commands)

        CmdAdd cmdName cmdText -> addCommand cmdName cmdText
        CmdRemove cmdName -> removeCommand cmdName
        CmdDynamic cmdName -> runDynCommand cmdName

lookupDynCmd :: Text -> Bot (Maybe Command)
lookupDynCmd cmdName = do
    s <- state
    cmds <- readTVarIOL (s^.dynamicCmds)

    case Map.lookup cmdName cmds of
        Nothing -> return Nothing
        _       -> return $ Just (CmdDynamic cmdName)

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
    cmds <- readTVarIOL (s^.dynamicCmds)

    case Map.lookup cmdName cmds of
        Nothing      -> return ()
        Just cmdText -> announce cmdText
