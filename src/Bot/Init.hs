{-# LANGUAGE OverloadedStrings #-}

module Bot.Init
    ( onConnect
    )
where

import           Control.Lens

import qualified Bot.Config                    as Config
import           Bot.Monad                      ( MonadBot )
import qualified Bot.Monad                     as Bot

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
