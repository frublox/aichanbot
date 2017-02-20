{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Bytes
import Data.Text (Text)
import qualified Data.Text as Text
import Control.Lens
import Text.Parsec
import Text.Parsec.Text
import Data.Monoid ((<>))
import Network.IRC.Client
import Control.Monad.IO.Class (liftIO)
import Data.List (intersperse)
import Data.Ini
import System.Exit

makeLenses ''Event
makeLenses ''InstanceConfig

data BotConfig = BotConfig
    { _botNick :: Text
    , _pass :: Text
    , _channel :: Text
    }
makeLenses ''BotConfig

data BotState = BotState
    { _config :: BotConfig
    }
makeLenses ''BotState

initBotState :: BotConfig -> BotState
initBotState conf = BotState
    { _config = conf
    }

data Command
    = CmdUnknown
    | CmdCommands
    | CmdHi

command :: Parser Command
command = do
    char '!'
    cmd <- many alphaNum

    case cmd of
        "commands" -> return CmdCommands
        "list" -> return CmdCommands
        "hi" -> return CmdHi
        _ -> return CmdUnknown

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

run :: BotConfig -> IO ()
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

privmsg :: Text -> Text -> StatefulIRC a ()
privmsg chan msg = send $ RawMsg ("PRIVMSG " <> chan <> " :" <> msg)

announce :: Text -> StatefulIRC BotState ()
announce msg = do
    s <- state
    privmsg (s^.config.channel) msg

replyTo :: Maybe Text -> Text -> StatefulIRC BotState ()
replyTo user msg = do
    s <- state
    privmsg (s^.config.channel) msg'

    where
        msg' = case user of
            Just user' -> "@" <> user' <> " " <> msg
            Nothing -> msg

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