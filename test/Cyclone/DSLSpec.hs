module Cyclone.DSLSpec where

import Control.Exception (evaluate)
import Cyclone.Parsing.DSL
import Cyclone.Parsing.Data
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
  let surroundedWilcardDsl = [Labeled Emoji Nothing, PlainText " **", Labeled Wildcard Nothing, PlainText "** ", Labeled Emoji Nothing]
  it "Should parse wildcards" $ do
    let result = parseDsl "{?}"
    result `shouldBe` Right [Labeled Wildcard Nothing]

  it "Should parse emojis" $ do
    let result = parseDsl "{emoji}"
    result `shouldBe` Right [Labeled Emoji Nothing]

  it "Should parse a simple emoji pattern" $ do
    let result = parseDsl "{emoji} **{?}** {emoji}"
    result `shouldBe` Right surroundedWilcardDsl

  it "Should parse placeholders with labels" $ do
    let result = parseDsl "{a: emoji} {category:  ?} {b:emoji}"
    result `shouldBe` Right [Labeled Emoji (Just "a"), PlainText " ", Labeled Wildcard (Just "category"), PlainText " ", Labeled Emoji (Just "b")]

  it "Should parse loops" $ do
    let result =
          parseDsl
            "Help:\n\
            \@loop\n\
            \{emoji} **{?}** {emoji}\n\
            \@loop"
    result `shouldBe` Right [PlainText "Help:\n", Loop (surroundedWilcardDsl ++ [PlainText "\n"])]

-- it "Should parse complex loops" $ do
--   let result =
--         parseDsl
--           "Help:\n\
--           \@loop\n\
--           \{emoji} **{?}** {emoji}\n\
--           \@loop"
--   result `shouldBe` Right [PlainText "Help:\n", Loop surroundedWilcardDsl]
