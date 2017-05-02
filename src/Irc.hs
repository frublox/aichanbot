{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}

module Irc where

import           Conduit

import           Data.ByteString           (ByteString)
import           Data.Char                 (isDigit)
import           Data.Conduit.Network
import           Data.Monoid               ((<>))
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import qualified Data.Text.IO              as Text

import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Trans.Class (lift)

import           Text.Megaparsec
import           Text.Megaparsec.Text

import           Bot
import           Irc.Types

handler :: [EventHandler] -> Conduit Text Bot Text
handler handlers = awaitForever $ \msg -> do
    let event = parseMaybe ircEvent msg

    maybe (err msg) (handleEvent msg) event

    where
        err msg = liftIO $ Text.putStrLn ("Badly formatted message from server: " <> msg)
        handleEvent msg event = do
            msgs <- forM handlers $ \(EventHandler e f) ->
                if e == event then lift (f msg) else return []
            yieldMany (concat msgs)

pingHandler :: EventHandler
pingHandler = EventHandler EPing $ \msg ->
    return ["PONG :" <> Text.drop 6 msg]

formatter :: Conduit Text Bot Text
formatter = awaitForever $ \msg ->
    yield (msg <> "\r\n")

serverLogger :: Conduit Text Bot Text
serverLogger = awaitForever $ \msg -> do
    liftIO $ Text.putStrLn ("> " <> msg)
    yield msg

clientLogger :: Conduit Text Bot Text
clientLogger = awaitForever $ \msg -> do
    liftIO $ Text.putStrLn ("< " <> msg)
    yield msg

onConnectC :: Bot [Text] -> Producer Bot Text
onConnectC msgs = lift msgs >>= yieldMany

runIrcBot :: Int -> ByteString -> IrcBot -> IO ()
runIrcBot port host ircBot = runTCPClient (clientSettings port host) app
    where
        app ad = runBot bot (ircBot^.ircBotConfig) (ircBot^.ircBotState)
            where
                bot = do
                    runConduit $
                        onConnectC (ircBot^.onConnect)
                        .| clientLogger
                        .| formatter
                        .| encodeUtf8C
                        .| appSink ad
                    runConduit $
                        appSource ad
                        .| decodeUtf8C
                        .| serverLogger
                        .| handler (ircBot^.eventHandlers)
                        .| clientLogger
                        .| formatter
                        .| encodeUtf8C
                        .| appSink ad
