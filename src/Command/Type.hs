{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Command.Type
    ( Command(..)
    ) where

import           GHC.Generics                    (Generic)

import           Control.Lens

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Char                       (toLower)
import           Data.Hashable                   (Hashable)
import           Data.Text                       (Text)
import qualified Data.Text                       as Text

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
    | Dynamic Text
    deriving (Eq, Generic, Hashable)

instance Show Command where
    show Cmds          = "cmds"
    show Hi            = "hi"
    show Bye           = "bye"
    show Aliases       = "aliases"
    show Help          = "help"
    show AddCmd        = "add"
    show RemoveCmd     = "remove"
    show EightBall     = "8ball"
    show (Dynamic txt) = show txt
