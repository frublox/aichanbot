{-# LANGUAGE OverloadedStrings #-}

module Util where

import           Data.Text              (Text)
import qualified Data.Text              as Text

textContains :: Text -> Text -> Bool
textContains y x = case Text.breakOn x y of
    (_, "") -> False
    _       -> True
