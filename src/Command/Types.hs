{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Command.Types where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Text     (Text)

import           Bot.Types     (Bot)
import           Control.Lens

data CmdPermissions
    = PermAnyone
    | PermModOnly
    deriving (Show, Eq)

instance FromJSON CmdPermissions where
    parseJSON = withText "Permission" $ \val ->
        case val of
            "anyone"  -> return PermAnyone
            "modonly" -> return PermModOnly
            _         -> fail "Invalid permission"

instance ToJSON CmdPermissions where
    toJSON PermAnyone  = String "anyone"
    toJSON PermModOnly = String "modonly"

-- data Command
--     = CmdUnknown
--     | CmdError Text Text
--     | CmdCommands
--     | CmdHi (Maybe Text)
--     | CmdBye (Maybe Text)
--     | CmdAdd Text Text
--     | CmdRemove Text
--     | CmdDynamic Text
--     | CmdHelp Text
--     | CmdAliases Text
--     deriving (Show, Eq)

data Invocation = Invocation
    { _invocOf   :: Command
    , _cmdSource :: Text
    , _arguments :: [Text]
    , _result    :: Bot ()
    }
makeLenses ''Invocation

data Command = Command
    { _info  :: CommandInfo
    , runCmd :: Text -> [Text] -> Invocation
    }
makeLenses ''Command

data CommandInfo = CommandInfo
    { _name        :: Text
    , _aliases     :: [Text]
    , _permissions :: CmdPermissions
    , _help        :: Text
    }
makeClassy ''CommandInfo

$(deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''CommandInfo)

makeCommand :: Text -> [Text] -> CmdPermissions -> (Text -> [Text] -> Bot ()) -> Command
makeCommand cmdName cmdAliases perms cmdResult = Command
    $ CommandInfo cmdName cmdAliases perms
    $ \source args ->
        Invocation
            (makeCommand cmdName cmdAliases perms cmdResult)
            source args (cmdResult source args)
