module Cyclone.Parsing.Parser where

import Cyclone.Parsing.Data
  ( DSL (..),
    LabeledVariable (..),
    ParsedMessage (ParsedMessage),
    Parser,
  )
import Cyclone.Parsing.Token
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
  ( MonadParsec (lookAhead, takeWhileP, try),
    ParseErrorBundle,
    anySingle,
    many,
    manyTill,
    oneOf,
    optional,
    parse,
    satisfy,
    (<|>),
  )
import Text.Megaparsec.Char (alphaNumChar, char, space, string)

type Consumer = Parser [ParsedMessage]

notColons :: Parser T.Text
notColons =
  let tokenLabel = Just "not-colon (not ':')"
   in takeWhileP tokenLabel (/= ':')

betweenColons :: Parser T.Text -> Parser T.Text
betweenColons a = colon *> a <* colon

emoji :: Parser T.Text
emoji = do
  let regularEmoji = betweenColons notColons
  let customDiscordEmoji = char '<' *> regularEmoji <* many alphaNumChar <* char '>'
  -- let unicodeEmoji = takeWhileP Nothing isUnicodeEmoji
  customDiscordEmoji <|> regularEmoji

listParser :: T.Text -> T.Text -> Parser T.Text
listParser delimiter pattern = do
  undefined

fromDsl :: [DSL] -> DSL -> Parser [ParsedMessage]
fromDsl next = \case
  Loop content ->
    dslToParser content
  t@(Labeled List {delimiter, pattern} _) ->
    return . ParsedMessage t <$> listParser delimiter pattern
  t@(Labeled Emoji _) ->
    return . ParsedMessage t <$> emoji
  t@(Labeled Wildcard _) ->
    return . ParsedMessage t . T.pack <$> wildcard
    where
      remainingInput = lookAhead (dslToParser next)
      -- keep consuming characters one-by-one until the rest of the parsers in line all succeed
      wildcard = manyTill anySingle remainingInput
  t@(PlainText text) ->
    return . ParsedMessage t <$> skipSpaces (string text)

dslToParser :: [DSL] -> Consumer
dslToParser [] = mempty
dslToParser (x : xs) = do
  result <- try $ fromDsl xs x
  rest <- dslToParser xs
  return $ result <> rest

parseMessageContent :: [DSL] -> T.Text -> Either (ParseErrorBundle T.Text Void) [ParsedMessage]
parseMessageContent dsl = parse (dslToParser dsl) ""
