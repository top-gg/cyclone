module Cyclone.Parsing.Parser where

import Cyclone.Parsing.Data
  ( DSL (..),
    LabeledVariable (..),
    ParsedMessage (ParsedMessage),
    Parser,
  )
import Cyclone.Parsing.Token (colon, skipSpaces)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
  ( MonadParsec (lookAhead, takeWhileP, try),
    ParseErrorBundle,
    anySingle,
    many,
    manyTill,
    parse,
    (<|>),
  )
import Text.Megaparsec.Char (alphaNumChar, char, string)

type Consumer = Parser [ParsedMessage]

notColons :: Parser T.Text
notColons =
  let tokenLabel = Just "not-colon (not ':')"
   in takeWhileP tokenLabel (/= ':')

betweenColons :: Parser T.Text -> Parser T.Text
betweenColons a = colon *> a <* colon

-- TODO: add parsing for unicode emojis
emoji :: Parser T.Text
emoji = do
  let regularEmoji = betweenColons notColons
  let customDiscordEmoji = char '<' *> regularEmoji <* many alphaNumChar <* char '>'
  customDiscordEmoji <|> regularEmoji

-- | A list parser that looks something like {commands: @list(",", "`?`")}
-- TODO: implement this
listParser :: T.Text -> T.Text -> Parser T.Text
listParser _delimiter _pattern = do
  undefined

-- | Convert a list of `DSL` tokens into `ParsedMessage` by passing
-- | the remaining list of tokens that are yet to be parsed
fromDsl :: [DSL] -> DSL -> Parser [ParsedMessage]
fromDsl next = \case
  Loop content ->
    dslToParser content
  t@(Labeled List {delimiter, pattern} _) ->
    return . ParsedMessage t <$> listParser delimiter pattern
  t@(Labeled Emoji _) ->
    return . ParsedMessage t <$> emoji
  t@(Labeled Wildcard _) -> do
    let remainingInput = lookAhead (dslToParser next)
    -- keep consuming characters one-by-one until the rest of the parsers in line all succeed
    let wildcard = manyTill anySingle remainingInput
    return . ParsedMessage t . T.pack <$> wildcard
  t@(PlainText text) ->
    return . ParsedMessage t <$> skipSpaces (string text)

dslToParser :: [DSL] -> Consumer
dslToParser [] = mempty
dslToParser (x : xs) = do
  result <- try $ fromDsl xs x
  rest <- dslToParser xs
  return $ result <> rest

type ParseResult = Either (ParseErrorBundle T.Text Void) [ParsedMessage]

parseMessageContent :: [DSL] -> T.Text -> ParseResult
parseMessageContent dsl = parse (dslToParser dsl) ""
