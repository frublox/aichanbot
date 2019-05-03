module Irc.EventHandler
    ( EventHandler(..)
    ) where

import           Data.Text (Text)

import           Irc.Event (Event)

data EventHandler m = EventHandler
    { getEvent    :: Event
    , handlerFunc :: Text -> m ()
    }
