{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Cyclone.Parsing.DSL (parseDsl, dslParser) where

import Cyclone.Parsing.Data (DSL (..), Parser)
import qualified Data.Text as T
import Data.Void
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

variable :: Parser p -> Parser p
variable = between (char '{') (char '}')

parseMaybeLabel :: Parser (Maybe T.Text)
parseMaybeLabel =
  let name = takeWhileP Nothing (/= ':')
   in optional $ try (name <* char ':' <* space)

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
