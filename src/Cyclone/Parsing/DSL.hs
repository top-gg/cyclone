{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Cyclone.Parsing.DSL (parseDsl, extractTemplatableVariable, dslParser) where

import Control.Monad (void)
import Cyclone.Parsing.Data (DSL (..), LabeledVariable (Emoji, Wildcard), Parser)
import Cyclone.Parsing.Token
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
  ( MonadParsec (eof, lookAhead, takeWhileP, try),
    ParseErrorBundle,
    between,
    manyTill,
    optional,
    parse,
    (<|>),
  )
import Text.Megaparsec.Char (char, space, string)
import Text.Megaparsec.Debug

variable :: Parser p -> Parser p
variable = between leftBracket rightBracket

parseMaybeLabel :: Parser (Maybe T.Text)
parseMaybeLabel =
  -- TODO: This is very hacky lol this parser should not have to know about the delimiters outside its own scope like '}'
  let name = takeWhileP Nothing (\e -> e /= ':' && e /= '}')
   in optional $ try (name <* char ':' <* try space)

listParser :: Parser LabeledVariable
listParser = do
  undefined

withLabel :: Parser LabeledVariable -> Parser DSL
withLabel parser = do
  binding <- parseMaybeLabel
  object <- parser
  return $ Labeled object binding

emoji :: Parser LabeledVariable
emoji = Emoji <$ string "emoji"

wildcard :: Parser LabeledVariable
wildcard = Wildcard <$ string "?"

expression :: Parser DSL
expression = variable (try (withLabel emoji) <|> try (withLabel wildcard))

dslContent :: Parser a -> Parser [DSL]
dslContent = manyTill (loop <|> expression <|> plainText)

loop :: Parser DSL
loop = do
  void loopMarker
  -- skip optional whitespace around the loop contents
  -- TODO: Move this eager space gobbling to `Cyclone.Parsing.Token.skipSpaces`
  space
  content <- dslContent (lookAhead . try $ loopMarker)
  space
  void loopMarker
  return $ Loop content
  where
    loopMarker = string "@loop"

plainText :: Parser DSL
plainText = PlainText <$> takeWhileP Nothing (\c -> c /= '{' && c /= '@')

dslParser :: Parser [DSL]
dslParser = dslContent eof

-- | Turns a YAML text input into a parser.
-- | Strips all whitespace from the input.
parseDsl :: T.Text -> Either (ParseErrorBundle T.Text Void) [DSL]
parseDsl text = parse dslParser "" (T.strip text)

extractTemplatableVariable :: DSL -> Maybe T.Text
extractTemplatableVariable (Labeled _ (Just key)) = return key
extractTemplatableVariable _ = Nothing
