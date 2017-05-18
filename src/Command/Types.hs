{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Command.Types where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Text     (Text)

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
            perm      -> fail $ "Invalid permission: '" <> perm <> "'"

instance ToJSON CmdPermissions where
    toJSON PermAnyone  = String "anyone"
    toJSON PermModOnly = String "modonly"

data CommandInfo = CommandInfo
    { _name        :: Text
    , _aliases     :: [Text]
    , _permissions :: CmdPermissions
    , _help        :: Text
    }
makeClassy ''CommandInfo

$(deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''CommandInfo)

data Command = Command
    { _info  :: CommandInfo
    , runCmd :: Text -> [Text] -> Invocation
    }

data Invocation = Invocation
    { _invocOf   :: Command
    , _cmdSource :: Text
    , _arguments :: [Text]
    , _result    :: (Monad m) => m ()
    }

makeLenses ''Command
makeLenses ''Invocation

makeCommand :: Monad m =>
    (Text -> [Text] -> m ())
    -> CommandInfo
    -> Command
makeCommand cmdResult cmdInfo = Command
    $ CommandInfo cmdName cmdAliases perms helpStr
    $ \source args ->
        Invocation
            (makeCommand cmdName cmdAliases perms cmdResult)
            source args (cmdResult source args)
