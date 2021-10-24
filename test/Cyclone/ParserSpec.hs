module Cyclone.ParserSpec where

import Control.Exception (evaluate)
import Cyclone.Parsing.DSL (parseDsl)
import Cyclone.Parsing.Data
import Cyclone.Parsing.Parser
import Data.Either
import Data.List
import qualified Data.Text as T
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

unwrap = fromRight (error "")

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

  let newline = PlainText "\n"
  let emojis = [emoji, emoji, emoji]
  let loopBaseDsl = [Loop (intersperse newline emojis)]
  it "full parses looped messages" $ do
    let result = intersperse (ParsedMessage newline "\n") $ map (`ParsedMessage` emojiName) emojis
    parseMessageContent loopBaseDsl (emojiStr <> "\n" <> emojiStr <> "\n" <> emojiStr) `shouldBe` Right result

  it "Should parse loops" $ do
    let finalTestString = PlainText "\ntest"
    let twoEmojis = [emoji, emoji]
    let dsl = [Loop (intersperse newline twoEmojis)] <> [finalTestString]
    let result = intersperse (ParsedMessage newline "\n") (map (`ParsedMessage` emojiName) twoEmojis) <> [ParsedMessage finalTestString "\ntest"]
    parseMessageContent dsl (emojiStr <> "\n" <> emojiStr <> "\ntest") `shouldBe` Right result

  it "should match a basic musicium help command" $ do
    let content = "**[Invite me with __Slash Commands__ Permissions](https://discord.com/api/oauth2/authorize?client_id=892341258503217163&permissions=8&scope=bot%20applications.commands), cause all of my Commands are available as Slash Commands too!**\n\n> Check out the [**Dashboard**](https://Musicium.oxytomato.repl.co/dashboard/333949691962195969) or the [**Live Music Queue**](https://Musicium.oxytomato.repl.co/queue/333949691962195969)"
    -- leading and trailing slashes to simulate a YAML file read
    let rawDsl = "\n{?}\n> Check out the [**Dashboard**]({?}) or the [**Live Music Queue**]({?})\n"
    let out = unwrap (parseDsl rawDsl)
    parseMessageContent out (T.strip content) `shouldSatisfy` isRight

  it "should extract wildcards from matched commands" $ do
    let content = "This is (a help command) with some (interesting patterns)"
    -- leading and trailing slashes to simulate a YAML file read
    let rawDsl = "This is ({first: ?}) with some ({second: ?})"
    let out = unwrap (parseDsl rawDsl)
    extractVariables (unwrap $ parseMessageContent out content)
      `shouldBe` [("first", "a help command"), ("second", "interesting patterns")]
