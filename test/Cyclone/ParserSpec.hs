module Cyclone.ParserSpec where

import Control.Exception (evaluate)
import Cyclone.Parsing.DSL (parseDsl)
import Cyclone.Parsing.Data
import Cyclone.Parsing.Parser
import Data.Either
import Data.List
import System.Exit (exitFailure)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Text.Megaparsec (errorBundlePretty)
import Text.Megaparsec.Error (errorBundlePretty)

unwrap = fromRight (error "")

printFailure result =
  case result of
    Right _ -> putStrLn ""
    Left a -> putStrLn $ errorBundlePretty a

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
    let result = parseMessageContent out content
    -- case result of
    --   Right _ -> print "a"
    --   Left a -> putStrLn $ errorBundlePretty a
    result `shouldSatisfy` isRight

  it "should extract wildcards from matched commands" $ do
    let content = "This is (a help command) with some (interesting patterns)"
    -- leading and trailing slashes to simulate a YAML file read
    let rawDsl = "This is ({first: ?}) with some ({second: ?})"
    let out = unwrap (parseDsl rawDsl)
    extractVariables (unwrap $ parseMessageContent out content)
      `shouldBe` [("first", "a help command"), ("second", "interesting patterns")]

  it "should match discord musicbot embed" do
    let content = "`>bassboost <none|low|medium|high>` - Enables bass boosting audio effect\n`>bump` - Moves a track to the front of the queue.\n`>clear` - Clears the server queue\n`>config` - Edit the bot settings\n`>disconnect` - Stop the music and leave the voice channel\n`>grab` - Saves the current song to your Direct Messages\n`>help [command]` - Information about the bot\n`>invite` - To invite me to your server\n`>loop` - Loop the current song\n`>loopqueue` - Loop the whole queue\n`>lyrics [Song Name]` - Shows the lyrics of the song searched\n`>move` - Moves a track to a specified position.\n`>nowplaying` - See what song is currently playing\n`>pause` - Pauses the music\n`>play [song]` - Play your favorite songs\n`>queue` - Shows all currently enqueued songs\n`>remove [number]` - Remove a song from the queue\n`>resume` - Resumes the music\n`>search [song]` - Shows a result of songs based on the search query\n`>seek <time s/m/h>` - Seek to a position in the song\n`>shuffle` - Shuffles the queue\n`>skip` - Skip the current song\n`>skipto <number>` - Skip to a song in the queue\n`>stats` - Get information about the bot\n`>volume <volume>` - Check or change the current volume\n`>youtube` - Starts a YouTube Together session\n \n Discord Music Bot Version: v4.1.2\n  [\10024 Support Server](https://discord.gg/sbySMS7m3v) | [GitHub](https://github.com/SudhanPlayz/Discord-MusicBot) | [Dashboard](http://localhost) | By [SudhanPlayz](https://github.com/SudhanPlayz)"
    -- let content = "`>bassboost <none|low|medium|high>` - Enables bass boosting audio effect\n`>youtube` - Starts a YouTube Together session\n\nDiscord Music Bot Version: v4.1.2\n[\10024 Support Server](https://discord.gg/sbySMS7m3v) | [GitHub](https://github.com/SudhanPlayz/Discord-MusicBot) | [Dashboard](http://localhost) | By [SudhanPlayz](https://github.com/SudhanPlayz)"
    let rawDsl =
          "@loop\n\
          \`>{commandName: ?}` - {?}\n\
          \@loop\n\
          \Discord Music Bot Version: {version: ?}\n\
          \{?}"
    let out = unwrap $ parseDsl rawDsl
    print out
    let result = parseMessageContent out content

    printFailure result
    result `shouldSatisfy` isRight

--  TODO: single loop statements don't work for some reason
-- it "it should match a single loop statement" do
--   let content = "`>bassboost <none|low|medium|high>` - Enables bass boosting audio effect\n`>youtube` - Starts a YouTube Together session\n"
--   let rawDsl =
--         "@loop\n\
--         \`>{commandName: ?}` - {?}\n\
--         \@loop\n"
--   let out = unwrap $ parseDsl rawDsl
--   print out
--   let result = parseMessageContent (out) content
--   printFailure result
--   result `shouldSatisfy` isRight
