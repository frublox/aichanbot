{-# LANGUAGE OverloadedStrings #-}

module Util where

import           Data.Text              (Text)
import qualified Data.Text              as Text

import           Control.Concurrent.STM

import           Text.Megaparsec

readAllTChan :: TChan a -> STM [a]
readAllTChan chan = do
    maybeX <- tryReadTChan chan
    case maybeX of
        Nothing -> return []
        Just x  -> do
            rest <- readAllTChan chan
            return (x : rest)

runParserTMaybe :: Monad m => ParsecT e s m a -> s -> m (Maybe a)
runParserTMaybe p s = do
    result <- runParserT p "" s
    return $ either (const Nothing) Just result

textContains :: Text -> Text -> Bool
textContains y x = case Text.breakOn x y of
    (_, "") -> False
    _       -> True
