{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}

module Irc
    ( module ReExported
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
import           Irc.Handlers              as ReExported
import           Irc.Parser                as ReExported
import           Irc.Types                 as ReExported
import           Lifted                    (atomicallyL)
import           Util                      (readAllTChan)

handler :: [EventHandler] -> Conduit Text Bot Text
handler handlers = awaitForever $ \msg -> do
    let event = parse ircEvent "" msg

    either (liftIO . die . parseErrorPretty) (runHandlers msg) event

    output <- lift (view outputChan)
    replies <- atomicallyL (readAllTChan output)
    yieldMany replies

    where
        runHandlers msg event = forM_ handlers (handleEvent event msg)
        handleEvent event msg (EventHandler e f) =
            when (e == event) $ lift (f msg)

newlineStripper :: Conduit Text Bot Text
newlineStripper = awaitForever $ \msg -> do
    let msg' = Text.dropEnd 2 msg
    yield msg'

newlineAdder :: Conduit Text Bot Text
newlineAdder = awaitForever $ \msg ->
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
                    .| newlineAdder
                    .| encodeUtf8C
                    .| appSink ad
