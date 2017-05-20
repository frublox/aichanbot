{-# LANGUAGE OverloadedStrings #-}

module Command.Parser where

import qualified Data.Map.Strict           as Map
import           Data.Text                 (Text)
import qualified Data.Text                 as Text

import           Control.Lens              hiding (noneOf)
import           Control.Monad.Trans.Class

import           Text.Megaparsec
import           Text.Megaparsec.Text

import           Bot
import           Commands
import           Types

invocation :: Text -> ParsecT Dec Text Bot Invocation
invocation source = do
    char '!'
    cmdName <- Text.pack <$> many alphaNumChar
    args <- fmap (map Text.pack) $ space *> many (quotedStr <|> word <* space)

    dynCmd <- lift (isDynCmd cmdName)

    if dynCmd
        then
             return $ runCmd (cmdDynamic cmdName) source args
        else do

            cmd <- lift (lookupCommand cmdName)

            maybe (fail "could not find cmd") (return . \c -> runCmd c source args) cmd

    where
        isDynCmd :: Text -> Bot Bool
        isDynCmd cmdName = do
            cmds <- use dynamicCmds

            case Map.lookup cmdName cmds of
                Nothing -> return False
                _       -> return True

        quotedStr :: Monad m => ParsecT Dec Text m String
        quotedStr = char '\"' *> many (noneOf ['\"']) <* char '\"'

        word :: Monad m => ParsecT Dec Text m String
        word = someTill anyChar (skipSome (oneOf [' ']) <|> eof)

commandName :: Parser Text
commandName = Text.pack <$> (char '!' *> many alphaNumChar)
