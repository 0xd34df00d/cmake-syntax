{-# LANGUAGE RecordWildCards, QuasiQuotes #-}

module Language.CMake.Parser
( fileParser
) where

import qualified Data.ByteString.Char8 as BS
import Control.Applicative
import Data.Char
import Data.Functor
import Data.Maybe
import Text.RawString.QQ
import Text.Trifecta

import Language.CMake.AST hiding(commandInvocation)

fileParser :: Parser File
fileParser = File <$> many fileElement

fileElement :: Parser FileElement
fileElement = (bracketComment <|> void space) *> lineEnding $> NonCommandElement
          <|> (CommandElement <$> commandInvocation) <* lineEnding

lineEnding :: Parser ()
lineEnding = skipOptional lineComment >> void newline

commandInvocation :: Parser CommandInvocation
commandInvocation = do
  void $ many space
  commandId <- BS.pack <$> some ('A' ~~ 'Z' <|> 'a' ~~ 'z' <|> char '_' <|> '0' ~~ '9')
  void $ many space
  commandArgs <- parens arguments
  pure CommandInvocation { .. }
  where (~~) = satisfyRange

arguments :: Parser [Argument]
arguments = do
  arg <- maybeToList <$> optional argument
  rest <- separatedArguments
  pure $ arg <> rest

separatedArguments :: Parser [Argument]
separatedArguments = maybeToList <$> try (some separation *> optional argument)
                 <|> many separation *> parens arguments

separation :: Parser ()
separation = void space <|> lineEnding

argument :: Parser Argument
argument = bracketArgument <|> quotedArgument <|> unquotedArgument

bracketArgument :: Parser Argument
bracketArgument = do
  opening <- char '[' *> many (char '=') <* char '['
  let closing = string $ "]" <> opening <> "]"
  argFromString <$> anyChar `manyTill` try closing

quotedArgument :: Parser Argument
quotedArgument = argFromString <$> many quotedElement `surroundedBy` char '"'

quotedElement :: Parser Char
quotedElement = noneOf [r|\"|] <|> escapeSequence <|> char '\\' *> newline

unquotedArgument :: Parser Argument
unquotedArgument = argFromString <$> some (satisfy unElem <|> escapeSequence)
  where unElem c = c `notElem` [r|()#\"|] && not (isSpace c)

escapeSequence :: Parser Char
escapeSequence = char '\\' >> (self <|> encoded)
  where
    self = oneOf [r|()#" \$@^;|]
    encoded = char 't' $> '\t'
          <|> char 'r' $> '\r'
          <|> char 'n' $> '\n'

bracketComment :: Parser ()
bracketComment = char '#' >> void bracketArgument

lineComment :: Parser ()
lineComment = char '#' >> void (many $ notChar '\n')
