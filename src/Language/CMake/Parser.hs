{-# LANGUAGE RecordWildCards, QuasiQuotes #-}

module Language.CMake.Parser
( fileParser
) where

import qualified Data.ByteString.Char8 as BS
import Control.Applicative
import Data.Char
import Data.Functor
import Data.Maybe
import Data.String
import Text.RawString.QQ
import Text.Trifecta

import Language.CMake.AST hiding(commandInvocation)

fileParser :: Parser File
fileParser = File <$> many fileElement <* eof

fileElement :: Parser FileElement
fileElement = try (many (try bracketComment <|> spaceNonLF) *> lineEnding $> NonCommandElement)
          <|> (CommandElement <$> commandInvocation) <* many spaceNonLF <* lineEnding
  where spaceNonLF = void $ satisfy $ \c -> isSpace c && c /= '\n'

lineEnding :: Parser ()
lineEnding = skipOptional lineComment >> void newline

commandInvocation :: Parser CommandInvocation
commandInvocation = do
  spaces
  commandId <- BS.pack <$> some ('A' ~~ 'Z' <|> 'a' ~~ 'z' <|> char '_' <|> '0' ~~ '9')
  spaces
  commandArgs <- between (char '(') (char ')') arguments
  pure CommandInvocation { .. }
  where (~~) = satisfyRange

arguments :: Parser [Argument]
arguments = do
  arg <- maybeToList <$> optional argument
  rest <- concat <$> many separatedArguments
  pure $ arg <> rest

separatedArguments :: Parser [Argument]
separatedArguments = maybeToList <$> try (some separation *> optional argument)
                 <|> many separation *> between (char '(') (char ')') arguments

separation :: Parser ()
separation = void space <|> lineEnding

argument :: Parser Argument
argument = bracketArgument <|> quotedArgument <|> unquotedArgument

bracketArgument :: Parser Argument
bracketArgument = do
  opening <- char '[' *> many (char '=') <* char '['
  let closing = string $ "]" <> opening <> "]"
  fromString <$> anyChar `manyTill` try closing

quotedArgument :: Parser Argument
quotedArgument = fromString . catMaybes <$> many quotedElement `surroundedBy` char '"'

quotedElement :: Parser (Maybe Char)
quotedElement = Just <$> (noneOf [r|\"|] <|> try escapeSequence)
            <|> (char '\\' *> newline $> Nothing)

unquotedArgument :: Parser Argument
unquotedArgument = fromString <$> some (satisfy unElem <|> escapeSequence)
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
