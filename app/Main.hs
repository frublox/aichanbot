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
import qualified Bot.Config          as Config
import qualified Bot.Env
import           Bot.EventHandlers   (msgHandler, pingHandler)
import           Bot.Monad           (MonadBot)
import qualified Bot.Monad           as Bot
import           Bot.Type            (runBot)
import qualified Irc
import           Util                (textContains)

port :: Int
port = 6667

hostname :: ByteString
hostname = "irc.chat.twitch.tv"

main :: IO ()
main = do
    env <- Bot.Env.init
    let handlers = [pingHandler, msgHandler]
    Irc.run port hostname (botC handlers onConnect) (runBot env)

-- TODO: Put this somewhere more appropriate
onConnect :: MonadBot m => m ()
onConnect = do
    conf <- Bot.getConfig
    let pass = view Config.pass conf
    let nick = view Config.nick conf
    let chan = view Config.channel conf

    Bot.sendMsg "CAP REQ :twitch.tv/tags"
    Bot.sendMsg ("PASS :" <> pass)
    Bot.sendMsg ("NICK :" <> nick)
    Bot.sendMsg ("JOIN :" <> chan)


