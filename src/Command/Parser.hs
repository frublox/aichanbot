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
import           Command.Commands
import           Command.Types

command :: Text -> ParsecT Dec Text Bot Invocation
command source = do
    char '!'
    cmdName <- many alphaNumChar

    dynCmd <- (lift . lookupDynCmd . Text.pack) cmdName

    case dynCmd of
        Just _ -> runCmd (cmdDynamic cmdName) source []
        Nothing -> do
            args <- fmap (map Text.pack) $ space *> many (quotedStr <|> word <* space)

            cmds <- lift (view botData.commands)

            cmd <- find
                (\cmd -> cmdName == cmd^.cmdInfo.name || cmdName `elem` cmd^.cmdInfo.aliases)
                cmds

            maybe (fail "could not find cmd") (return . \c -> runCmd c source args) cmd

    where
        stripAt s = case s of
            ('@':s') -> s'
            _        -> s

        lookupDynCmd :: Text -> Bot (Maybe Command)
        lookupDynCmd cmdName = do
            cmds <- use dynamicCmds

            case Map.lookup cmdName cmds of
                Nothing -> return Nothing
                _       -> return $ Just (CmdDynamic cmdName)

        quotedStr :: Monad m => ParsecT Dec Text m String
        quotedStr = char '\"' *> many (noneOf ['\"']) <* char '\"'

        word :: Monad m => ParsecT Dec Text m String
        word = someTill anyChar (skipSome (oneOf [' ']) <|> eof)

commandName :: Parser Text
commandName = Text.pack <$> (char '!' *> many alphaNumChar)
