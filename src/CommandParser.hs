module CommandParser where

import qualified Data.Map.Strict           as Map
import           Data.Text                 (Text)
import qualified Data.Text                 as Text

import           Control.Lens              hiding (noneOf)
import           Control.Monad.Trans.Class

import           Text.Megaparsec
import           Text.Megaparsec.Text

import           Network.IRC.Client

import           Bot
import           Command
import           Lifted

command :: ParsecT Dec Text Bot Command
command = do
    char '!'
    cmdName <- many alphaNumChar

    dynCmd <- (lift . lookupDynCmd . Text.pack) cmdName

    case dynCmd of
        Just _ -> (return . CmdDynamic . Text.pack) cmdName
        Nothing -> do
            args <- space *> many (quotedStr <|> word <* space)

            case cmdName of
                "commands" -> return CmdCommands
                "list" -> return CmdCommands
                "cmds" -> return CmdCommands

                "hi" -> case args of
                    (user:_) -> (return . CmdHi . Just . Text.pack . stripAt) user
                    _        -> return (CmdHi Nothing)

                "bye" -> case args of
                    (user:_) -> (return . CmdBye . Just . Text.pack . stripAt) user
                    _        -> return (CmdBye Nothing)

                "add" -> case args of
                    (name:cmdText:_) -> return $ CmdAdd (Text.pack name) (Text.pack cmdText)
                    _                   -> fail "!add takes two arguments"

                "remove" -> case args of
                    (name:_) -> (return . CmdRemove . Text.pack) name
                    _        -> fail "!remove takes one argument"

                _ -> return CmdUnknown

    where
        stripAt s = case s of
            ('@':s') -> s'
            _        -> s

        lookupDynCmd :: Text -> Bot (Maybe Command)
        lookupDynCmd cmdName = do
            s <- state
            cmds <- readTVarIOL (s^.dynamicCmds)

            case Map.lookup cmdName cmds of
                Nothing -> return Nothing
                _       -> return $ Just (CmdDynamic cmdName)

        quotedStr :: Monad m => ParsecT Dec Text m String
        quotedStr = char '\"' *> many (noneOf ['\"']) <* char '\"'

        word :: Monad m => ParsecT Dec Text m String
        word = someTill anyChar (skipSome (oneOf [' ']) <|> eof)

commandName :: Parser Text
commandName = Text.pack <$> (char '!' *> many alphaNumChar)
