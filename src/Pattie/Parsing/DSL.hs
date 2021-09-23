{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Pattie.Parsing.DSL (parseDsl) where

import qualified Data.Text as T
import Debug.Trace (traceIO, traceId)
import Pattie.Parsing.Data (DSL (..), Parser)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Debug
import Text.ParserCombinators.ReadP (many1)

variable :: Parser p -> Parser p
variable = between (char '{') (char '}')

emoji :: Parser DSL
emoji = Emoji <$ string "emoji"

word :: Parser T.Text
word = takeWhileP Nothing (== ' ') *> takeWhileP Nothing (/= ' ')

wildcard :: Parser DSL
wildcard = Wildcard <$ string "?"

expression :: Parser DSL
expression = variable (emoji <|> wildcard)

plainText :: Parser DSL
plainText = PlainText <$> takeWhileP Nothing (/= '{')

parseDsl :: Parser [DSL]
parseDsl = manyTill (expression <|> plainText) eof
