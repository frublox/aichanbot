{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Command.Permissions
    ( Permissions(..)
    , fromMsg
    )
where

import           Data.Aeson
import           Data.HashMap.Strict ((!))
import           Data.Text           (Text, unpack)
import           Text.Megaparsec     (parse)

import           Irc.Parser          (twitchTagsP)
import           Util                (textContains)

data Permissions
    = Anyone
    | Moderator
    | Caster
    deriving (Show, Eq, Ord)

instance FromJSON Permissions where
    parseJSON = withText "Permission" $ \case
        "anyone"    -> pure Anyone
        "moderator" -> pure Moderator
        "caster"    -> pure Caster
        perm        -> fail $ "Invalid permission: '" <> unpack perm <> "'"

instance ToJSON Permissions where
    toJSON Anyone    = String "anyone"
    toJSON Moderator = String "modonly"
    toJSON Caster    = String "caster"

fromMsg :: Text -> Permissions
fromMsg msg = case parse twitchTagsP "" msg of
    Right tags 
        | any (`textContains` "broadcaster") (tags ! "badges") -> Caster
        | "mod" `elem` (tags ! "user-type") -> Moderator
        | otherwise -> Anyone
    _ -> Anyone
