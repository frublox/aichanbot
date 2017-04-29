{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Command where

import           Data.Aeson
import           Data.Text    (Text)
import           GHC.Generics

import           Control.Lens

data CmdPermissions
    = PermAnyone
    | PermModOnly

instance FromJSON CmdPermissions where
    parseJSON = withText "Permission" $ \val ->
        case val of
            "anyone"  -> return PermAnyone
            "modonly" -> return PermModOnly
            _         -> fail "Invalid permission"

data Command
    = CmdUnknown
    | CmdCommands
    | CmdHi (Maybe Text)
    | CmdBye (Maybe Text)
    | CmdAdd Text Text
    | CmdRemove Text
    | CmdDynamic Text

data CommandInfo = CommandInfo
    { _aliases     :: [Text]
    , _permissions :: CmdPermissions
    } deriving (Generic)
makeClassy ''CommandInfo

instance FromJSON CommandInfo
