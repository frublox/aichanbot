{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Ini
import           Data.List                  (intersperse)
import           Data.Map.Strict            ((!))
import qualified Data.Map.Strict            as Map
import           Data.Monoid                ((<>))
import           Data.Text                  (Text)
import qualified Data.Text                  as Text

import           Control.Lens
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Either

import           System.Exit                (die)

import           Text.Megaparsec            (parse, parseErrorPretty,
                                             runParserT)

import           Bot
import           Command
import           Irc

main :: IO ()
main = do
    ini <- readIniFile "config.ini" >>= either die return
    botConfig <- initBotConfig ini >>= either die return

    let handlers = [pingHandler, msgHandler]
    let ircBot = IrcBot botSetup handlers botConfig initBotState

    runIrcBot 6667 "irc.chat.twitch.tv" ircBot

botSetup :: Bot ();
botSetup = do
    pass <- view botPass
    nick <- view botNick
    chan <- view channel

    send "CAP REQ :twitch.tv/tags"
    send ("PASS :" <> pass)
    send ("NICK :" <> nick)
    send ("JOIN :" <> chan)

msgHandler :: EventHandler
msgHandler = EventHandler EPrivMsg $ \ircMsg ->
    eitherT (liftIO . die . parseErrorPretty) return $ do
        source <- hoistEither (parse ircMsgSource "" ircMsg)
        text <- hoistEither (parse ircMsgText "" ircMsg)
        cmd <- EitherT (runParserT command "" text)

        lift (handleCmd source cmd)

handleCmd :: Text -> Command -> Bot ()
handleCmd source cmd = case cmd of
    CmdHi target  -> do
        msg <- views (botData . strings) (! "hi")
        liftIO $ print msg
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
    CmdRemove cmdName -> do
        dynamicCmds %= Map.delete cmdName
        msg <- views (botData . strings) (! "remove")
        replyTo source (msg <> cmdName)
    CmdDynamic cmdName -> do
        msg <- uses dynamicCmds (! cmdName)
        replyTo source msg
    _             -> return ()



-- onConnect :: Bot ()
-- onConnect = do
--     s <- state

--     let nickname = s^.config.botNick
--     let chan = s^.config.channel

--     send $ RawMsg ("PASS " <> s^.config.pass)
--     send (Nick nickname)
--     send $ RawMsg "CAP REQ :twitch.tv/tags"
--     send (Join chan)

--     return ()

-- msgHandler :: EventHandler BotState
-- msgHandler = EventHandler "bot" EEverything $ \event -> do
--     liftIO $ print (event^.raw)
--     case event^.message of
--         Privmsg _ (Right msg) -> do
--             let user = extractUser event
--             cmd <- runParserT command "" msg

--             mapM_ (handleCommand user) cmd

--         _ -> return ()

--     where
--         extractUser :: Event Text -> Maybe Text
--         extractUser event =
--             case event^.source of
--                 User user      -> Just user
--                 Channel _ user -> Just user
--                 _              -> Nothing

-- handleCommand :: Maybe Text -> Command -> Bot ()
-- handleCommand user cmd =
--     case cmd of
--         CmdUnknown -> replyTo user "idk that command :/"

--         CmdHi target -> case target of
--             Just _  -> replyTo target "hi! KonCha"
--             Nothing -> replyTo user "hi! KonCha"
--         CmdBye target -> case target of
--             Just _  -> replyTo target "cya! KonCha"
--             Nothing -> replyTo user "cya! KonCha"
--         CmdCommands -> do
--             s <- state

--             dynCmds <- fmap (map (cons '!')) getDynCommands
--             let commands = map (cons '!') $ Map.keys (s^.staticCmds)
--             let cmdList = (Text.concat . intersperse ", ") (commands <> dynCmds)

--             replyTo user cmdList

--         CmdAdd cmdName cmdText -> do
--             s <- state

--             cmdAliases <- getAliases
--             dynCmds <- getDynCommands

--             if Map.member cmdName (s^.staticCmds) || cmdName `elem` cmdAliases || cmdName `elem` dynCmds
--                 then
--                     replyTo user ("!" <> cmdName <> " is already the name of an existing command")
--                 else do
--                     addDynCommand cmdName cmdText
--                     replyTo user ("Added command !" <> cmdName)

--         CmdRemove cmdName -> do
--             removeDynCommand cmdName
--             replyTo user ("Removed command !" <> cmdName)
--         CmdDynamic cmdName -> runDynCommand cmdName

-- addDynCommand :: Text -> Text -> Bot ()
-- addDynCommand cmdName cmdText = do
--     s <- state
--     atomicallyL $ modifyTVar (s^.dynamicCmds) (Map.insert cmdName cmdText)

-- removeDynCommand :: Text -> Bot ()
-- removeDynCommand cmdName = do
--     s <- state
--     atomicallyL $ modifyTVar (s^.dynamicCmds) (Map.delete cmdName)

-- runDynCommand :: Text -> Bot ()
-- runDynCommand cmdName = do
--     s <- state
--     cmdMap <- readTVarIOL (s^.dynamicCmds)

--     mapM_ announce (Map.lookup cmdName cmdMap)

-- getDynCommands :: Bot [Text]
-- getDynCommands = do
--     s <- state
--     cmdMap <- readTVarIOL (s^.dynamicCmds)

--     return (Map.keys cmdMap)

-- getAliases :: Bot [Text]
-- getAliases = do
--     s <- state
--     return (concat $ (s^.staticCmds) ^.. traverse . aliases)
