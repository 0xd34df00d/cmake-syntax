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
main = hspec $
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
