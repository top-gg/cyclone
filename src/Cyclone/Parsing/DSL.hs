{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Cyclone.Parsing.DSL (parseDsl, dslParser, innerLoop) where

import Control.Applicative hiding (many, some)
import Control.Monad
import Cyclone.Parsing.Data (DSL (..), Parser)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
  ( MonadParsec (eof, lookAhead, takeWhileP, try),
    ParseErrorBundle,
    between,
    many,
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

innerLoop :: Parser [DSL]
innerLoop = dslParser

dslContent :: Parser a -> Parser [DSL]
dslContent = manyTill (loop <|> expression <|> plainText)

loop :: Parser DSL
loop = do
  void loopMarker
  space
  content <- dslContent (lookAhead . try $ space *> loopMarker)
  space
  void loopMarker
  return $ Loop content
  where
    loopMarker = string "@loop"

plainText :: Parser DSL
plainText = PlainText <$> takeWhileP Nothing (/= '{')

dslParser :: Parser [DSL]
dslParser = dslContent eof

parseDsl :: T.Text -> Either (ParseErrorBundle T.Text Void) [DSL]
parseDsl = parse dslParser ""
