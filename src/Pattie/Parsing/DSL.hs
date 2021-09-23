{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Pattie.Parsing.DSL (parseDsl, dslParser) where

import qualified Data.Text as T
import Data.Void
import Debug.Trace (traceIO, traceId)
import Pattie.Parsing.Data (DSL (..), Parser)
import Text.Megaparsec
  ( MonadParsec (eof, takeWhileP, try),
    ParseErrorBundle,
    between,
    manyTill,
    optional,
    parse,
    (<|>),
  )
import Text.Megaparsec.Char
import Text.Megaparsec.Debug
import Text.ParserCombinators.ReadP (many1)

variable :: Parser p -> Parser p
variable = between (char '{') (char '}')

parseMaybeLabel :: Parser (Maybe T.Text)
parseMaybeLabel = optional $ try (takeWhileP Nothing (/= ':') <* string ":" <* optional spaceChar)

emoji :: Parser DSL
emoji = do
  label <- parseMaybeLabel
  _ <- string "emoji"
  return $ Emoji {label}

wildcard :: Parser DSL
wildcard = do
  label <- parseMaybeLabel
  _ <- string "?"
  return $ Wildcard {label}

expression :: Parser DSL
expression = variable (try emoji <|> try wildcard)

plainText :: Parser DSL
plainText = PlainText <$> takeWhileP Nothing (/= '{')

dslParser :: Parser [DSL]
dslParser = manyTill (expression <|> plainText) eof

parseDsl :: T.Text -> Either (ParseErrorBundle T.Text Void) [DSL]
parseDsl = parse dslParser ""
