{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}

module Irc
    ( module ReExported
    , module Irc
    ) where

import           Conduit

import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Char8     as Bytes
import           Data.Conduit.Network
import           Data.Monoid               ((<>))
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Data.Text.Encoding

import           Control.Concurrent        (forkIO, threadDelay)
import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Trans.Class (lift)

import           System.Exit               (die)
import           Text.Megaparsec

import           Irc.Handlers              as ReExported
import           Irc.Parser                as ReExported
import           Irc.Types                 as ReExported

import           Lifted                    (atomicallyL, readTVarIOL)
import           Types
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
    liftIO $ Bytes.putStrLn $ encodeUtf8 ("--> " <> msg')
    yield msg

clientLogger :: Conduit Text Bot Text
clientLogger = awaitForever $ \msg -> do
    liftIO $ Bytes.putStrLn $ encodeUtf8 ("<-- " <> msg)
    yield msg

onConnectC :: Bot () -> Conduit Text Bot Text
onConnectC action = do
    lift action
    output <- lift (view outputChan)
    msgs <- atomicallyL (readAllTChan output)
    yieldMany msgs
    awaitForever $ \msg -> yield msg

rateLimiter :: Conduit Text Bot Text
rateLimiter = do
    msgsSent <- atomicallyL (newTVar (0 :: Int))

    let timer = forever $ do
            threadDelay (30 * 1000000)
            atomically (writeTVar msgsSent 0)

    liftIO (forkIO timer)

    let checkAndYield msg = do
            numMsgsSent <- readTVarIOL msgsSent
            liftIO $ print numMsgsSent
            if numMsgsSent < 10
                then do
                    atomicallyL (modifyTVar' msgsSent (+1))
                    yield msg
                else do
                    atomicallyL $ do
                        msgs <- readTVar msgsSent
                        unless (msgs == 0) retry
                    yield msg

    awaitForever checkAndYield

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
                    .| rateLimiter
                    .| clientLogger
                    .| newlineAdder
                    .| encodeUtf8C
                    .| appSink ad
