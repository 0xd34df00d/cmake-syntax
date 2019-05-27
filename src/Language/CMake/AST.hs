{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.CMake.AST where

import qualified Data.ByteString.Char8 as BS

newtype File = File { fileElements :: [FileElement] } deriving (Eq, Show)

data FileElement
  = CommandElement { commandInvocation :: CommandInvocation }
  | NonCommandElement
  deriving (Eq, Show)

data CommandInvocation = CommandInvocation
  { commandId :: BS.ByteString
  , commandArgs :: [Argument]
  }
  deriving (Eq, Show)

data LiteralElem
  = LiteralString { literalString :: BS.ByteString }
  | VariableReference { variableName :: BS.ByteString }
  deriving (Eq, Show)

newtype Literal = Literal { literalParts :: [LiteralElem] }
  deriving (Eq, Show, Semigroup, Monoid)

litFromString :: BS.ByteString -> Literal
litFromString = Literal . pure . LiteralString

newtype Argument = Argument { argumentLiteral :: Literal }
  deriving (Eq, Show, Semigroup, Monoid)

argFromString :: String -> Argument
argFromString = Argument . litFromString . BS.pack
