{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

import Test.Hspec
import Text.RawString.QQ
import Text.Trifecta.Parser
import Text.Trifecta.Result

import Language.CMake.AST
import Language.CMake.Parser

instance Eq a => Eq (Result a) where
  Success a == Success b = a == b
  _ == _ = False

(~~>) :: String -> File -> Expectation
(~~>) str res = parseString fileParser mempty (str <> "\n") `shouldBe` Success res

main :: IO ()
main = hspec $ do
  describe "Parsing simple commands" $ do
    it "without args"
        $ [r|add_executable()|]
      ~~> File [ CommandElement $ CommandInvocation "add_executable" [] ]
    it "with one arg"
        $ [r|add_executable(foo)|]
      ~~> File [ CommandElement $ CommandInvocation "add_executable" ["foo"] ]
    it "with several simple args"
        $ [r|add_executable(foo bar baz)|]
      ~~> File [ CommandElement $ CommandInvocation "add_executable" ["foo", "bar", "baz"] ]
    it "with leading spaces"
        $ [r|    add_executable()|]
      ~~> File [ CommandElement $ CommandInvocation "add_executable" [] ]
    it "empty line"
        $ [r||]
      ~~> File [ NonCommandElement ]
    it "empty line with spaces"
        $ "   \t  "
      ~~> File [ NonCommandElement ]
  describe "Parsing a sequence of commands" $ do
    it "without args"
        $ [r|add_executable()
             add_library()|]
      ~~> File [ CommandElement $ CommandInvocation "add_executable" []
               , CommandElement $ CommandInvocation "add_library" []
               ]
  describe "Parsing comments" $ do
    it "parses single whole-line comment"
        $ [r|# this is a comment|]
      ~~> File [NonCommandElement]
    it "parses multiple whole-line comments"
        $ [r|# this is a comment
             # and this is a continuation|]
      ~~> File [NonCommandElement, NonCommandElement]
    it "parses end-of-line comments"
        $ [r|add_library() # this is a comment|]
      ~~> File [CommandElement $ CommandInvocation "add_library" []]
