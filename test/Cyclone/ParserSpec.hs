module Cyclone.ParserSpec where

import Control.Exception (evaluate)
import Cyclone.Parsing.DSL
import Cyclone.Parsing.Data
import Cyclone.Parsing.Parser
import Data.Either
import Data.List
import Data.Maybe
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Text.Megaparsec

spec :: Spec
spec = describe "Parser generator" $ do
  let emoji = Labeled Emoji Nothing
  let emojiStr = "<:feelssighman:694837745398317116>"
  let emojiName = "feelssighman"
  it "Creates parser from basic DSL" $ do
    let dsl = [emoji, PlainText "**", Labeled Wildcard Nothing, PlainText "**", emoji]
    let inputText = ":emoji: **HELLO** :emoji2:"
    let result = parseMessageContent dsl inputText
    let expected = Right $ zipWith ParsedMessage dsl ["emoji", "**", "HELLO", "**", "emoji2"]
    result `shouldBe` expected

  it "doesn't parse malformed emojis" $ do
    parseMessageContent [emoji] "123141" `shouldSatisfy` (not . isRight)

  it "parses discord emojis" $ do
    parseMessageContent [emoji] emojiStr `shouldBe` Right [ParsedMessage emoji emojiName]

  -- it "parses unicode emojis" $ do
  --   parseMessageContent [emoji] "⚠" `shouldBe` Right [ParsedMessage emoji "⚠️"]

  it "full parses looped messages" $ do
    let newline = PlainText "\n"
    let emojis = [emoji, emoji, emoji]
    let dsl = [Loop (intersperse newline emojis)]
    let result = intersperse (ParsedMessage newline "\n") $ map (`ParsedMessage` emojiName) emojis
    parseMessageContent dsl (emojiStr <> "\n" <> emojiStr <> "\n" <> emojiStr) `shouldBe` Right result

-- it "parses discord message" $ do
--   let messagePattern =
--         "Help:\
--         \@loop\n\
--         \{emoji} **{?}** {emoji}\
--         \{?}\
--         \@loop"
--   let msg =
--         "Help:\
--         \<:feelssighman:694837745398317116> **Moderation** <:feelssighman:694837745398317116>\
--         \`ban`, `mute`"
--   let result = parseMessageContent (fromRight [] $ parseDsl messagePattern) msg
--   result `shouldSatisfy` isRight
