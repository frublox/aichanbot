{-# LANGUAGE OverloadedStrings #-}

module Command.Permissions
    ( Permissions(..)
    , fromText
    ) where

import           Data.Aeson
import           Data.HashMap.Strict ((!))
import           Data.Text           (Text, unpack)
import           Text.Megaparsec     (parse)

import           Irc.Parser          (twitchTagsP)
import           Util                (textContains)

data Permissions
    = Anyone
    | Moderator
    deriving (Show, Eq, Ord)

instance FromJSON Permissions where
    parseJSON = withText "Permission" $ \val ->
        case val of
            "anyone"  -> return Anyone
            "modonly" -> return Moderator
            perm      -> fail $ "Invalid permission: '" <> unpack perm <> "'"

instance ToJSON Permissions where
    toJSON Anyone    = String "anyone"
    toJSON Moderator = String "modonly"

fromText :: Text -> Permissions
fromText msg =
    case parse twitchTagsP "" msg of
        Right tags ->
            let hasMod = "mod" `elem` (tags ! "user-type")
                isCaster = any (`textContains` "broadcaster") (tags ! "badges")
            in  if hasMod || isCaster
                    then Moderator
                    else Anyone
        _ -> Anyone
