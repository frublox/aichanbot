{-# LANGUAGE TemplateHaskell #-}

module Bot.Config
    ( Config
    , nick
    , pass
    , channel
    ) where

import           Control.Lens

import           Data.Aeson.TH

import           Data.Text     (Text)

data Config = Config
    { _nick    :: Text
    , _pass    :: Text
    , _channel :: Text
    }

makeLenses ''Config
$(deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''Config)
