module Irc.Parser where

import           Data.Char            (isDigit)
import           Data.List            (all)
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Data.Text            (Text)

import           Text.Megaparsec
import           Text.Megaparsec.Text

import           Irc.Types

ircEvent :: Parser IrcEvent
ircEvent = do
    skipMany (char ':')
    char ':'
    skipMany spaceChar
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
    s <- some (noneOf [' '])
    twitchTags' s

    where
        twitchTags' = do
            tags <- some (noneOf [';']) `sepBy` char ';'
            tags' <- mapM twitchTag tags
            return (Map.fromList tags')

twitchTag :: Parser (Text, [Text])
twitchTag = do
    key <- some (noneOf ['='])
    char '='
    values <- some (noneOf [',']) `sepBy` char ','
    return (key, values)


