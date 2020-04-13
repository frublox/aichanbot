{-# LANGUAGE OverloadedStrings #-}

module Bot.Init
    ( onConnect
    )
where

import qualified Control.Lens as Lens
import           Data.Text    (Text)

import qualified Bot.Config   as Config
import           Bot.Monad    (MonadBot)
import qualified Bot.Monad    as Bot

onConnect :: MonadBot m => m [Text]
onConnect = do
    conf <- Bot.getConfig
    pure 
        [ "CAP REQ :twitch.tv/tags"
        , "PASS :" <> Lens.view Config.pass conf
        , "NICK :" <> Lens.view Config.nick conf
        , "JOIN :" <> Lens.view Config.channel conf
        ]