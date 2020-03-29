{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Command.Permissions
    ( Permissions(..)
    , fromMsg
    )
where

import           Data.Aeson
import           Data.HashMap.Strict            ( (!) )
import           Data.Text                      ( Text
                                                , unpack
                                                )
import           Text.Megaparsec                ( parse )

import           Irc.Parser                     ( twitchTagsP )
import           Util                           ( textContains )

data Permissions
    = Anyone
    | Moderator
    deriving (Show, Eq, Ord)

instance FromJSON Permissions where
    parseJSON = withText "Permission" $ \case
        "anyone"    -> return Anyone
        "moderator" -> return Moderator
        perm        -> fail $ "Invalid permission: '" <> unpack perm <> "'"

instance ToJSON Permissions where
    toJSON Anyone    = String "anyone"
    toJSON Moderator = String "modonly"

fromMsg :: Text -> Permissions
fromMsg msg = case parse twitchTagsP "" msg of
    Right tags ->
        let hasMod   = "mod" `elem` (tags ! "user-type")
            isCaster = any (`textContains` "broadcaster") (tags ! "badges")
        in  if hasMod || isCaster then Moderator else Anyone
    _ -> Anyone
