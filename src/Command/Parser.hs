{-# LANGUAGE OverloadedStrings #-}

module Command.Parser
    ( commandFromNameP
    , commandP
    , unprefixedCommandP
    , maybePrefixedCommandP
    , argsP
    )
where

import           Control.Applicative            ( liftA2
                                                , (<|>)
                                                )
import           Control.Monad                  ( void )
import           Control.Monad.Trans.Class      ( lift )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Void                      ( Void )
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           Command.Type                   ( Command )
import qualified Command.Type                  as Cmd

type Parser = Parsec Void Text
type ParserM = ParsecT Void Text

commandFromNameP :: Parser Command
commandFromNameP = do
    cmdName <- many alphaNumChar
    pure $ case cmdName of
        "hi"       -> Cmd.Hi
        "bye"      -> Cmd.Bye
        "commands" -> Cmd.Cmds
        "add"      -> Cmd.AddCmd
        "remove"   -> Cmd.RemoveCmd
        "help"     -> Cmd.Help
        "aliases"  -> Cmd.Aliases
        "8ball"    -> Cmd.EightBall
        str        -> Cmd.Dynamic (Text.pack str)

commandP :: Monad m => (Text -> m (Maybe Command)) -> ParserM m Command
commandP resolveCmd = do
    char '!'
    unprefixedCommandP resolveCmd

unprefixedCommandP
    :: Monad m => (Text -> m (Maybe Command)) -> ParserM m Command
unprefixedCommandP resolveCmd = do
    cmdName <- many alphaNumChar
    result  <- lift $ resolveCmd (Text.pack cmdName)
    case result of
        Nothing  -> fail ("Couldn't resolve command: " <> cmdName)
        Just cmd -> pure cmd

maybePrefixedCommandP
    :: Monad m => (Text -> m (Maybe Command)) -> ParserM m Command
maybePrefixedCommandP resolveCmd =
    try (commandP resolveCmd) <|> unprefixedCommandP resolveCmd

argsP :: Parser [Text]
argsP = do
    char '!'
    many alphaNumChar
    space
    args <- many (quotedStr <|> word <* space)
    pure $ fmap Text.pack args
  where
    quote :: Char
    quote = '\"'

    quotedStr :: Parser String
    quotedStr = between (char quote) (char quote) (many (anySingleBut '\"'))

    word :: Parser String
    word = someTill anySingle (space1 <|> eof)

-- commandWithArgsP :: Monad m => (Text -> m (Maybe Command)) -> Parser m (Command, [Text])
-- commandWithArgsP resolveCmd = liftA2 (,) (commandP) argsP
