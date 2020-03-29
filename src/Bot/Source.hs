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
fromText txt | Text.null txt        = Source txt
             | Text.head txt == '@' = Source (Text.tail txt)
             | otherwise            = Source txt

toText :: Source -> Text
toText (Source txt) = txt
