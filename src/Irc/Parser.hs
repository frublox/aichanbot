{-# LANGUAGE OverloadedStrings #-}

module Irc.Parser
    ( eventP
    , twitchTagP
    , twitchTagsP
    , msgTextP
    , msgSourceP
    )
where

import           Bot.Source                     ( Source )
import qualified Bot.Source                    as Source
import           Data.Char                      ( isDigit )
import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict           as HashMap
import           Data.List                      ( all )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Void                      ( Void )
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           Irc.Event                      ( Event(..) )

type Parser = Parsec Void Text

eventP :: Parser Event
eventP = do
    skipMany twitchTagsP
    space
    let src = char ':' *> some (noneOf [' ']) <* spaceChar
    skipMany src
    space
    event <- manyTill alphaNumChar spaceChar

    pure $ case event of
        "PRIVMSG" -> EPrivMsg
        "NOTICE"  -> ENotice
        "NICK"    -> ENick
        "JOIN"    -> EJoin
        "PART"    -> EPart
        "QUIT"    -> EQuit
        "MODE"    -> EMode
        "TOPIC"   -> ETopic
        "INVITE"  -> EInvite
        "KICK"    -> EKick
        "PING"    -> EPing
        "PONG"    -> EPong
        _         -> if all isDigit event then ENumeric else ERawMsg

twitchTagP :: Parser (Text, [Text])
twitchTagP = do
    key <- some (noneOf ['='])
    char '='
    values <- many (noneOf [',', ' ', ';']) `sepBy` char ','
    return (Text.pack key, map Text.pack values)

twitchTagsP :: Parser (HashMap Text [Text])
twitchTagsP = do
    char '@'
    tags <- twitchTagP `sepBy` char ';'
    return (HashMap.fromList tags)

msgTextP :: Parser Text
msgTextP = do
    manyTill anySingle (try (string "PRIVMSG"))
    space
    manyTill (noneOf [' ']) spaceChar
    space
    char ':'
    msg <- manyTill anySingle eof
    return (Text.pack msg)

msgSourceP :: Parser Source
msgSourceP = do
    skipMany twitchTagsP
    let src = char ':' *> some (noneOf ['!']) <* char '!'
    someTill anySingle (lookAhead src)
    Source.fromText . Text.pack <$> src
