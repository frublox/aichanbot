module Irc.EventHandler
    ( EventHandler(..)
    )
where

import           Data.Text (Text)

import           Irc.Event (Event)

data EventHandler m = EventHandler
    { getEvent    :: Event
    -- A handler takes a message and returns 0 or more messages to send back
    , handlerFunc :: Text -> m [Text]
    }
