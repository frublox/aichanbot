module Irc.Parser where

import           Data.Char            (isDigit)
import           Data.List            (all)
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Data.Text            (Text)
import qualified Data.Text            as Text

import           Text.Megaparsec
import           Text.Megaparsec.Text

import           Irc.Types

ircEvent :: Parser IrcEvent
ircEvent = do
    skipMany twitchTags
    space
    let src = char ':' *> some (noneOf [' ']) <* spaceChar
    skipMany src
    space
    event <- manyTill alphaNumChar spaceChar

    case event of
        "PRIVMSG" -> return EPrivMsg
        "NOTICE" -> return ENotice
        "NICK" -> return ENick
        "JOIN" -> return EJoin
        "PART" -> return EPart
        "QUIT" -> return EQuit
        "MODE" -> return EMode
        "TOPIC" -> return ETopic
        "INVITE" -> return EInvite
        "KICK" -> return EKick
        "PING" -> return EPing
        "PONG" -> return EPong

        _ ->
            if all isDigit event
                then return ENumeric
                else return ERawMsg

twitchTags :: Parser (Map Text [Text])
twitchTags = do
    char '@'
    tags <- twitchTag `sepBy` char ';'
    return (Map.fromList tags)

twitchTag :: Parser (Text, [Text])
twitchTag = do
    key <- some (noneOf ['='])
    char '='
    values <- many (noneOf [',', ' ', ';']) `sepBy` char ','
    return (Text.pack key, map Text.pack values)

ircMsgText :: Parser Text
ircMsgText = do
    manyTill anyChar (try (string "PRIVMSG"))
    space
    manyTill (noneOf [' ']) spaceChar
    space
    char ':'
    msg <- manyTill anyChar eof
    return (Text.pack msg)

ircMsgSource :: Parser Text
ircMsgSource = do
    skipMany twitchTags
    let src = char ':' *> some (noneOf ['!']) <* char '!'
    someTill anyChar (lookAhead src)
    Text.pack <$> src
