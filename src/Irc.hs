{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}

module Irc
    ( module Irc.Types
    , module Irc.Parser
    , module Irc
    ) where

import           Conduit

import           Data.ByteString           (ByteString)
import           Data.Conduit.Network
import           Data.Monoid               ((<>))
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import qualified Data.Text.IO              as Text

import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Trans.Class (lift)

import           System.Exit               (die)
import           Text.Megaparsec

import           Bot
import           Irc.Parser
import           Irc.Types
import           Lifted                    (atomicallyL)
import           Util                      (readAllTChan)

handler :: [EventHandler] -> Conduit Text Bot Text
handler handlers = awaitForever $ \msg -> do
    let event = parse ircEvent "" msg

    either (liftIO . die . parseErrorPretty) (runHandlers msg) event

    output <- lift (view outputChan)
    responses <- atomicallyL (readAllTChan output)
    yieldMany responses

    where
        runHandlers msg event = forM_ handlers (handleEvent event msg)
        handleEvent event msg (EventHandler e f) =
            when (e == event) $ lift (f msg)

pingHandler :: EventHandler
pingHandler = EventHandler EPing $ \msg ->
    send ("PONG :" <> Text.drop 6 msg)

newlineStripper :: Conduit Text Bot Text
newlineStripper = awaitForever $ \msg -> do
    let msg' = Text.replace "\r\n" "" msg
    yield msg'

formatter :: Conduit Text Bot Text
formatter = awaitForever $ \msg ->
    yield (msg <> "\r\n")

serverLogger :: Conduit Text Bot Text
serverLogger = awaitForever $ \msg -> do
    let msg' = Text.replace "\n" "\n--> " msg
    liftIO $ Text.putStrLn ("--> " <> msg')
    yield msg

clientLogger :: Conduit Text Bot Text
clientLogger = awaitForever $ \msg -> do
    liftIO $ Text.putStrLn ("<-- " <> msg)
    yield msg

onConnectC :: Bot () -> Conduit Text Bot Text
onConnectC action = do
    lift action
    output <- lift (view outputChan)
    msgs <- atomicallyL (readAllTChan output)
    yieldMany msgs
    awaitForever $ \msg -> yield msg

runIrcBot :: Int -> ByteString -> IrcBot -> IO ()
runIrcBot port host ircBot = runTCPClient (clientSettings port host) app
    where
        app ad = runBot bot (ircBot^.ircBotConfig) (ircBot^.ircBotState)
            where
                bot = runConduit $
                    appSource ad
                    .| decodeUtf8C
                    .| newlineStripper
                    .| serverLogger
                    .| handler (ircBot^.eventHandlers)
                    .| onConnectC (ircBot^.onConnect)
                    .| clientLogger
                    .| formatter
                    .| encodeUtf8C
                    .| appSink ad
