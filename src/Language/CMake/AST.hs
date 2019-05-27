module Language.CMake.AST where

import qualified Data.ByteString.Char8 as BS

newtype File = File { fileElements :: [FileElement] } deriving (Eq, Show)

newtype FileElement = FileElement
  { commandInvocation :: CommandInvocation
  }
  deriving (Eq, Show)

data CommandInvocation = CommandInvocation
  { identifier :: BS.ByteString
  , arguments :: [Argument]
  }
  deriving (Eq, Show)

data LiteralElem
  = LiteralString { literalString :: BS.ByteString }
  | VariableReference { variableName :: BS.ByteString }
  deriving (Eq, Show)

newtype Argument = Argument
  { argument :: BS.ByteString
  }
  deriving (Eq, Show)
