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
  ( MonadParsec (eof, lookAhead, takeWhileP, try),
    ParseErrorBundle,
    anySingle,
    many,
    manyTill,
    parse,
    satisfy,
    skipMany,
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

skipSpaceCharacters :: Parser ()
skipSpaceCharacters = skipMany (char ' ')

-- | Convert a list of `DSL` tokens into `ParsedMessage` by passing
-- | the remaining list of tokens that are yet to be parsed
fromDsl :: Bool -> [DSL] -> DSL -> Parser [ParsedMessage]
fromDsl insideLoop next = \case
  Loop content ->
    -- if there's nothing else after the loop we should be trying to match
    -- the loop parser until the end of the input
    let nextParser =
          case next of
            [] -> [] <$ eof
            _ -> dslToParser next True
        tryNextParserInLine = lookAhead $ try nextParser
     in (join <$> manyTill (dslToParser content True) tryNextParserInLine)
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
    -- Sometimes bot embeds do their newlines like "\n  " or "\n My commands" for no reason
    -- These need to be consumed automatically before the plaintext itself is consumed in order not to fail
    let baseParser = traverse (\e -> if e == '\n' then skipSpaceCharacters *> char e <* skipSpaceCharacters else char e) $ T.unpack text
        parser = if insideLoop then baseParser else skipSpaces baseParser
     in -- Very hacky way to make sure we don't gobble sensitive spaces when we're inside a loop
        return . ParsedMessage t . T.pack <$> parser

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
