{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Command.Info
    ( CommandInfo(..)
    , name
    , aliases
    , permissions
    , help
    , dynCmdInfo
    )
where

import           Control.Lens
import           Data.Aeson.TH
import           Data.Text                      ( Text )

import           Command.Permissions            ( Permissions )
import qualified Command.Permissions           as Perms

data CommandInfo = CommandInfo
    { _name        :: Text
    , _aliases     :: [Text]
    , _permissions :: Permissions
    , _help        :: Text
    }

makeLenses ''CommandInfo
$(deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''CommandInfo)

dynCmdInfo :: Text -> CommandInfo
dynCmdInfo txt = CommandInfo
    { _name        = txt
    , _aliases     = []
    , _permissions = Perms.Anyone
    , _help        = "Usage !" <> txt <> " [optional username, w/ or w/out @]"
    }
