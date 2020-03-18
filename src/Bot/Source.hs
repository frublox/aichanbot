module Bot.Source
    ( Source
    , fromText
    , toText
    )
where

import           Data.Text                      ( Text )
import qualified Data.Text                     as Text

newtype Source = Source Text

fromText :: Text -> Source
fromText = Source

toText :: Source -> Text
toText (Source txt) = txt