{-# LANGUAGE OverloadedStrings #-}

module Command.Parser
  ( commandFromNameP,
    commandP,
    unprefixedCommandP,
    maybePrefixedCommandP,
    argsP,
  )
where

import           Command.Type              (Command)
import qualified Command.Type              as Cmd
import           Control.Applicative       (liftA2, (<|>))
import           Control.Monad             (void)
import           Control.Monad.Trans.Class (lift)
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Data.Void                 (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

type Parser = Parsec Void Text
type ParserM = ParsecT Void Text

commandFromNameP :: Parser Command
commandFromNameP = do
    cmdName <- many alphaNumChar
    pure $ Cmd.fromText (Text.pack cmdName)

commandP :: Monad m => (Text -> m (Maybe Command)) -> ParserM m Command
commandP resolveCmd = do
    char '!'
    unprefixedCommandP resolveCmd

unprefixedCommandP :: Monad m => (Text -> m (Maybe Command)) -> ParserM m Command
unprefixedCommandP resolveCmd = do
    cmdName <- many alphaNumChar
    result <- lift $ resolveCmd (Text.pack cmdName)
    case result of
        Nothing  -> fail ("Couldn't resolve command: " <> cmdName)
        Just cmd -> pure cmd

maybePrefixedCommandP :: Monad m => (Text -> m (Maybe Command)) -> ParserM m Command
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
        -- Parses the contents of a string in double quotes.
        quotedStr :: Parser String
        quotedStr = between (char '\"') (char '\"') (many (anySingleBut '\"'))

        word :: Parser String
        word = someTill anySingle (space1 <|> eof)
