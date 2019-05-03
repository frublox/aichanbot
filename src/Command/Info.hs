{-# LANGUAGE TemplateHaskell #-}

module Command.Info
    ( CommandInfo(..)
    , name
    , aliases
    , permissions
    , help
    ) where

import           Control.Lens
import           Data.Aeson.TH
import           Data.Text           (Text)

import           Command.Permissions (Permissions)

data CommandInfo = CommandInfo
    { _name        :: Text
    , _aliases     :: [Text]
    , _permissions :: Permissions
    , _help        :: Text
    }

makeLenses ''CommandInfo
$(deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''CommandInfo)
