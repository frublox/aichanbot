{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module Irc where

import           Conduit

import           Data.ByteString        (ByteString)
import           Data.Char              (isDigit)
import           Data.Conduit.Network
import           Data.Map.Strict        (Map)
import           Data.Monoid            ((<>))
import           Data.String            (IsString)
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Data.Text.Encoding     (decodeUtf8, encodeUtf8)
import qualified Data.Text.IO           as Text

import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.IO.Class (liftIO)

import           Text.Megaparsec
import           Text.Megaparsec.Text

import           Lifted                 (atomicallyL)

data IrcEvent
    = EPrivMsg
    | ENotice
    | ENick
    | EJoin
    | EPart
    | EQuit
    | EMode
    | ETopic
    | EInvite
    | EKick
    | EPing
    | EPong
    | ENumeric
    | ERawMsg
    deriving (Eq, Show)

ircEvent :: Parser IrcEvent
ircEvent = do
    skipMany (char ':')
    char ':'
    skipMany spaceChar
    space
    event <- manyTill alphaNumChar spaceChar
    case event of
        "PRIVMSG" -> return EPrivMsg
        "NOTICE" -> return ENotice
        "NICK" -> return ENick
        "JOIN" -> return EJoin
        "PART" -> return EPart
        "QUIT" -> return EQuit
        "MODE" -> return EMode
        "TOPIC" -> return ETopic
        "INVITE" -> return EInvite
        "KICK" -> return EKick
        "PING" -> return EPing
        "PONG" -> return EPong

        _ -> if all isDigit event
            then return ENumeric
            else return ERawMsg

data EventHandler = EventHandler IrcEvent (Text -> IO Text)

handler :: [EventHandler] -> Conduit Text IO Text
handler eventHandlers = awaitForever $ \msg ->
    case parseMaybe ircEvent msg of
        Nothing -> liftIO $ Text.putStrLn ("Badly formatted message from server: " <> msg)
        Just event -> forM_ eventHandlers $ \h ->
            case h of
                (EventHandler event f) -> yieldM (f msg)
                _                      -> return ()

pingHandler :: EventHandler
pingHandler = EventHandler EPing $
    \msg -> return $ "PONG :" <> Text.drop 6 msg

formatter :: Conduit Text IO Text
formatter = awaitForever $
    \msg -> yield (msg <> "\r\n")

runIrcBot :: MonadIO io => Int -> ByteString -> [EventHandler] -> io ()
runIrcBot port host eventHandlers = do
    input <- atomicallyL newTChan
    output <- atomicallyL newTChan

    liftIO $ runTCPClient (clientSettings port host) (app eventHandlers)

    where
        app eventHandlers ad = runConduit $
            appSource ad
            .| decodeUtf8C
            .| handler eventHandlers
            .| formatter
            .| encodeUtf8C
            .| appSink ad
