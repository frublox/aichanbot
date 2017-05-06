module Util where

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
