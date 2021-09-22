module Pattie.Parsing.Parser where

import qualified Data.Text as T
import Data.Void
import Pattie.Parsing.DSL
import Pattie.Parsing.Data (DSL (..), Parser)
import Text.Megaparsec
import Text.Megaparsec.Char

type Consumer = Parser T.Text

emojiDelimiter :: Parser Char
emojiDelimiter = char ':'

emoji :: Consumer
emoji = between emojiDelimiter emojiDelimiter (takeWhileP Nothing (/= ':'))

fromDsl :: DSL -> Consumer
fromDsl = \case
  Emoji -> emoji
  PlainText text -> string text
  Wildcard {parser} -> parser

dslToParser :: [DSL] -> Consumer
dslToParser = foldMap fromDsl
