module Pattie.DSLSpec where

import Control.Exception (evaluate)
import Pattie.Parsing.DSL
import Pattie.Parsing.Data
import Test.Hspec
  ( Spec,
    anyException,
    describe,
    it,
    shouldBe,
    shouldThrow,
  )
import Text.Megaparsec

spec :: Spec
spec = describe "DSL Parser" $ do
  it "Should parse wildcards" $ do
    let result = parseDsl "{?}"
    result `shouldBe` Right [Wildcard Nothing]

  it "Should parse emojis" $ do
    let result = parseDsl "{emoji}"
    result `shouldBe` Right [Emoji Nothing]

  it "Should parse a simple emoji pattern" $ do
    let result = parseDsl "{emoji} **{?}** {emoji}"
    result `shouldBe` Right [Emoji Nothing, PlainText " **", Wildcard Nothing, PlainText "** ", Emoji Nothing]

  it "Should parse placeholders with labels" $ do
    let result = parseDsl "{a: emoji} {category:?} {b: emoji}"
    result `shouldBe` Right [Emoji (Just "a"), PlainText " ", Wildcard (Just "category"), PlainText " ", Emoji (Just "b")]
