{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.ByteString   (ByteString)
import           Data.Text         (Text)

import           Bot.Conduit       (botC)
import qualified Bot.Env           as Env
import           Bot.EventHandlers (msgHandler, pingHandler)
import           Bot.Init          (onConnect)
import           Bot.Type          (runBot)
import qualified Irc

port :: Int
port = 6667

hostname :: ByteString
hostname = "irc.chat.twitch.tv"

main :: IO ()
main = do
    env <- Env.init
    let handlers = [pingHandler, msgHandler]
    Irc.run port hostname (botC handlers onConnect) (runBot env)
