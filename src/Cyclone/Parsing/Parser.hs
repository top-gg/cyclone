module Cyclone.Parsing.Parser where

import Control.Monad
import Cyclone.Parsing.DSL
import Cyclone.Parsing.Data
  ( DSL (..),
    EmbeddedVariable,
    LabeledVariable (..),
    ParsedMessage (ParsedMessage),
    Parser,
  )
import Cyclone.Parsing.Token (colon, skipSpaces)
import qualified Data.Text as T
import Data.Void (Void)
import Debug.Trace (traceShowId)
import Text.Megaparsec
  ( MonadParsec (lookAhead, takeWhileP, try),
    ParseErrorBundle,
    anySingle,
    many,
    manyTill,
    parse,
    (<|>),
  )
import Text.Megaparsec.Char (alphaNumChar, char, space, string)
import Text.Megaparsec.Debug

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
fromDsl :: Bool -> [DSL] -> DSL -> Parser [ParsedMessage]
fromDsl insideLoop next = \case
  Loop content ->
    (join <$> manyTill (dslToParser content True) ((dslToParser next True)))
  t@(Labeled List {delimiter, pattern} _) ->
    return . ParsedMessage t <$> listParser delimiter pattern
  t@(Labeled Emoji _) ->
    return . ParsedMessage t <$> emoji
  t@(Labeled Wildcard _) -> do
    let remainingInput = lookAhead (dslToParser next insideLoop)
    -- keep consuming characters one-by-one until the rest of the parsers in line all succeed
    let wildcard = manyTill anySingle remainingInput
    return . ParsedMessage t . T.pack <$> wildcard
  t@(PlainText text) ->
    -- Very hacky way to make sure we don't gobble sensitive spaces when we're inside a loop
    return . ParsedMessage t <$> if insideLoop then string text else skipSpaces (string text)

dslToParser :: [DSL] -> Bool -> Consumer
dslToParser [] _ = mempty
dslToParser (x : xs) insideLoop = do
  result <- try $ fromDsl insideLoop xs x
  rest <- dslToParser xs insideLoop
  return $ result <> rest

type ParseResult = Either (ParseErrorBundle T.Text Void) [ParsedMessage]

-- | Attempt to parse message from an array of Domain Specific Languages
-- | Strips out leading and trailing whitespace from the incoming message, expects the
-- | Parser to also ignore the same whitespace restrictions
parseMessageContent :: [DSL] -> T.Text -> ParseResult
parseMessageContent dsl message = parse (dslToParser dsl False) "" (T.strip message)

extractVariables :: [ParsedMessage] -> [EmbeddedVariable]
extractVariables (ParsedMessage dsl value : xs) =
  case extractTemplatableVariable dsl of
    Just key -> (key, value) : extractVariables xs
    Nothing -> extractVariables xs
extractVariables [] = []
