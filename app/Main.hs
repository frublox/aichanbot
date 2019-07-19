{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative (liftA2)
import           Control.Lens
import           Control.Monad       (forM_, when)
import           Data.Aeson.Lens
import           Data.ByteString     (ByteString)
import qualified Data.HashMap.Strict as HashMap
import           Data.Text           (Text)

import           Bot.Conduit         (botC)
import qualified Bot.Env             as Env
import           Bot.EventHandlers   (msgHandler, pingHandler)
import           Bot.Init            (onConnect)
import           Bot.Type            (runBot)
import qualified Irc
import           Util                (textContains)

port :: Int
port = 6667

hostname :: ByteString
hostname = "irc.chat.twitch.tv"

main :: IO ()
main = do
    env <- Env.init
    let handlers = [pingHandler, msgHandler]
    Irc.run port hostname (botC handlers onConnect) (runBot env)
