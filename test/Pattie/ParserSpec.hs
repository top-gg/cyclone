module Pattie.ParserSpec where

import Control.Exception (evaluate)
import Pattie.Parsing.Data
import Pattie.Parsing.Parser
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Megaparsec

spec :: Spec
spec = describe "Parsing DSL patterns" $ do
  it "Creates parser from basic DSL" $
    do
      let emoji = Emoji Nothing
      let dsl = [emoji, PlainText " **", Wildcard Nothing, PlainText "** ", emoji]
      let inputText = ":emoji: **HELLO** :emoji2:"
      let result = parseDsl dsl inputText
      let expected =
            map
              (uncurry ParsedMessage)
              [ (emoji, "emoji"),
                ((PlainText " **"), " **"),
                ((Wildcard Nothing), "HELLO"),
                ((PlainText "** "), "** "),
                (emoji, "emoji2")
              ]
      result
        `shouldBe` Right expected
