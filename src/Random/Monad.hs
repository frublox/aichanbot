module Random.Monad
    ( MonadRandom
    , randomR
    )
where

import           Control.Monad.IO.Class         ( MonadIO )
import           System.Random                  ( Random )

class (MonadIO m) => MonadRandom m where
    randomR :: Random a => (a, a) -> m a
