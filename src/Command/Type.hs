{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Command.Type
    ( Command(..)
    , toText
    , fromText
    )
where

import           GHC.Generics                    (Generic)

import           Control.Lens

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Char                       (toLower)
import           Data.Hashable                   (Hashable)
import           Data.HashMap.Strict             (HashMap, (!))
import qualified Data.HashMap.Strict             as HashMap
import           Data.Text                       (Text)

import           Text.ParserCombinators.ReadPrec (look)
import           Text.Read                       (Read, readPrec)

import           Command.Info                    (CommandInfo)

data Command
    = Cmds
    | Hi
    | Bye
    | Aliases
    | Help
    | AddCmd
    | RemoveCmd
    | EightBall
    | Reload
    | Dynamic Text
    deriving (Eq, Generic, Hashable, Show)

cmdToName :: HashMap Command Text
cmdToName = HashMap.fromList
    [ (Cmds, "cmds")
    , (Hi, "hi")
    , (Bye, "bye")
    , (Aliases, "aliases")
    , (Help, "help")
    , (AddCmd, "add")
    , (RemoveCmd, "remove")
    , (EightBall, "8ball")
    , (Reload, "reload")
    ]

nameToCmd :: HashMap Text Command
nameToCmd = HashMap.fromList $
    (\(x, y) -> (y, x)) <$> HashMap.toList cmdToName

toText :: Command -> Text
toText (Dynamic txt) = txt
toText cmd           = cmdToName ! cmd

fromText :: Text -> Command
fromText txt = HashMap.lookupDefault (Dynamic txt) txt nameToCmd
