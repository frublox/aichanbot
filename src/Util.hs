{-# LANGUAGE OverloadedStrings #-}

module Util where

import           Control.Lens

import           Data.Aeson.Lens
import Data.Aeson.Types (Value)
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text

import           Control.Concurrent.STM

readAllTChan :: TChan a -> STM [a]
readAllTChan chan = do
    maybeX <- tryReadTChan chan
    case maybeX of
        Nothing -> return []
        Just x  -> do
            rest <- readAllTChan chan
            return (x : rest)

textContains :: Text -> Text -> Bool
textContains y x = case Text.breakOn x y of
    (_, "") -> False
    _       -> True
