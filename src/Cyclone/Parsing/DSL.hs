{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Cyclone.Parsing.DSL (parseDsl, dslParser, innerLoop) where

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

variable :: Parser p -> Parser p
variable = between leftBracket rightBracket

parseMaybeLabel :: Parser (Maybe T.Text)
parseMaybeLabel =
  let name = takeWhileP Nothing (/= ':')
   in optional $ try (name <* char ':' <* space)

listParser :: Parser LabeledVariable
listParser = do
  undefined

withLabel :: Parser LabeledVariable -> Parser DSL
withLabel parser = do
  binding <- parseMaybeLabel
  parsed <- parser
  return $ Labeled parsed binding

emoji :: Parser LabeledVariable
emoji = Emoji <$ string "emoji"

wildcard :: Parser LabeledVariable
wildcard = Wildcard <$ string "?"

expression :: Parser DSL
expression = variable (try (withLabel emoji) <|> try (withLabel wildcard))

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
plainText = PlainText <$> takeWhileP Nothing (\c -> c /= '{' && c /= '@')

dslParser :: Parser [DSL]
dslParser = dslContent eof

parseDsl :: T.Text -> Either (ParseErrorBundle T.Text Void) [DSL]
parseDsl = parse dslParser ""
