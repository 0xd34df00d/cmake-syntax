{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

import Data.Char
import Test.Hspec
import Text.RawString.QQ
import Text.Trifecta.Parser
import Text.Trifecta.Result

import Language.CMake.AST
import Language.CMake.Parser

instance Eq a => Eq (Result a) where
  Success a == Success b = a == b
  Failure _ == Failure _ = True
  _ == _ = False

(~~>) :: HasCallStack => String -> File -> Expectation
(~~>) str res = parseString fileParser mempty (str <> "\n") `shouldBe` Success res

dropLeading :: String -> String
dropLeading str = init $ unlines $ l : (drop numSpaces <$> init ls)
  where
    (l:ls) = lines str
    numSpaces = length $ takeWhile isSpace $ last ls

main :: IO ()
main = hspec $ do
  describe "Parsing simple commands" $ do
    it "without args"
        $ [r|add_executable()|]
      ~~> File [ CommandElement $ CommandInvocation "add_executable" [] ]
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
  describe "Bracket comments" $ do
    it "parses simplest empty bracket comments"
        $ [r|#[[]]|]
      ~~> File [NonCommandElement]
    it "parses zero-eq bracket comments with text"
        $ [r|#[[ foobar ]]|]
      ~~> File [NonCommandElement]
    it "parses non-zero-eq bracket comments with text"
        $ [r|#[==[ foobar ]==]|]
      ~~> File [NonCommandElement]
    it "parses non-zero-eq bracket comments with nested eqs (less)"
        $ [r|#[==[ ]=] ]==]|]
      ~~> File [NonCommandElement]
    it "parses non-zero-eq bracket comments with nested eqs (more)"
        $ [r|#[==[ ]===] ]==]|]
      ~~> File [NonCommandElement]
    it "parses multilinenon-zero-eq bracket comments"
        $ [r|#[==[ foobar
             ]=]
             fdsafsdahljkrew hjkfdshafkjlashjk
             ]===]
             ]==]|]
      ~~> File [NonCommandElement]
  describe "Parsing simple command arguments" $ do
    it "parses command with one arg"
        $ [r|add_executable(foo)|]
      ~~> File [ CommandElement $ CommandInvocation "add_executable" ["foo"] ]
    it "parses command with several simple args"
        $ [r|add_executable(foo bar baz)|]
      ~~> File [ CommandElement $ CommandInvocation "add_executable" ["foo", "bar", "baz"] ]
  describe "Parsing bracket command arguments" $ do
    it "parses command with bracket arguments"
        $ dropLeading
          [r|add_executable([==[
            This is a bracket argument
            ]==])
            |]
      ~~> File [ CommandElement $ CommandInvocation "add_executable" ["\nThis is a bracket argument\n"] ]
    it "parses command with multiline bracket arguments"
        $ dropLeading
          [r|add_executable([==[
            This is a
            bracket
            argument
            ]==])
            |]
      ~~> File [ CommandElement $ CommandInvocation "add_executable" ["\nThis is a\nbracket\nargument\n"] ]
    it "does not escape in bracket arguments"
        $ dropLeading
          [r|add_executable([==[
            This is \t \; \n a \- bracket argument \meh
            ]==])
            |]
      ~~> File [ CommandElement $ CommandInvocation "add_executable" ["\nThis is \\t \\; \\n a \\- bracket argument \\meh\n"] ]
    it "does not expand variables in bracket arguments"
        $ dropLeading
          [r|add_executable([==[
            This is a ${bracket} argument
            ]==])
            |]
      ~~> File [ CommandElement $ CommandInvocation "add_executable" ["\nThis is a ${bracket} argument\n"] ]
  describe "Parsing quoted command arguments" $ do
    it "parses command with one arg"
        $ [r|add_executable("foo")|]
      ~~> File [ CommandElement $ CommandInvocation "add_executable" ["foo"] ]
    it "parses command with multple args"
        $ [r|add_executable("foo" "bar")|]
      ~~> File [ CommandElement $ CommandInvocation "add_executable" ["foo", "bar"] ]
    it "does escaping"
        $ [r|add_executable("foo \n \; bar")|]
      ~~> File [ CommandElement $ CommandInvocation "add_executable" ["foo \n ; bar"] ]
    it "parses command with multiline arg"
        $ dropLeading
          [r|add_executable("foo
             bar
             baz")
             |]
      ~~> File [ CommandElement $ CommandInvocation "add_executable" ["foo\nbar\nbaz"] ]
    it "parses command with multiline arg and final line feed"
        $ dropLeading
          [r|add_executable("foo
             bar
             baz
             ")
             |]
      ~~> File [ CommandElement $ CommandInvocation "add_executable" ["foo\nbar\nbaz\n"] ]
    it "parses command with single-line arg spanning multiple lines"
        $ dropLeading
          [r|add_executable("foo \
             bar \
             baz")
             |]
      ~~> File [ CommandElement $ CommandInvocation "add_executable" ["foo bar baz"] ]
