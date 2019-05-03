{-# LANGUAGE OverloadedStrings #-}

module Command.Parser
    ( commandP
    , argsP
    , commandWithArgsP
    ) where

import           Control.Applicative  (liftA2)

import           Data.Text            (Text)
import qualified Data.Text            as Text

import           Text.Megaparsec
import           Text.Megaparsec.Char

import           Command.Type         (Command)
import qualified Command.Type         as Cmd

type Parser = Parsec () Text

commandP :: Parser Command
commandP = do
    cmdName <- char '!' *> many alphaNumChar
    pure $ case cmdName of
        "cmds"    -> Cmd.Cmds
        "hi"      -> Cmd.Hi
        "bye"     -> Cmd.Bye
        "aliases" -> Cmd.Aliases
        "help"    -> Cmd.Help
        "add"     -> Cmd.AddCmd
        "remove"  -> Cmd.RemoveCmd
        "8ball"   -> Cmd.EightBall
        txt       -> Cmd.Dynamic (Text.pack txt)

argsP :: Parser [Text]
argsP = do
    args <- space *> many (quotedStr <|> word <* space)
    pure $ fmap Text.pack args
    where
        quotedStr :: Parser String
        quotedStr = char '\"' *> many (anySingleBut '\"') <* char '\"'

        word :: Parser String
        -- TODO: Use space instead of skipSome (oneOf [' '])?
        word = someTill anySingle (skipSome (oneOf [' ']) <|> eof)

commandWithArgsP :: Parser (Command, [Text])
commandWithArgsP = liftA2 (,) commandP argsP
