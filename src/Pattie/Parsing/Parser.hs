module Pattie.Parsing.Parser where

import qualified Data.Text as T
import Data.Void
import Pattie.Parsing.DSL
import Pattie.Parsing.Data (DSL (..), ParsedMessage (ParsedMessage), Parser)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Debug (dbg)

type Consumer = Parser [ParsedMessage]

emojiDelimiter :: Parser Char
emojiDelimiter = char ':'

emoji :: Parser T.Text
emoji = between emojiDelimiter emojiDelimiter (takeWhileP Nothing (/= ':'))

fromDsl :: [DSL] -> DSL -> Parser ParsedMessage
fromDsl next t =
  ParsedMessage t <$> case t of
    Emoji _ -> emoji
    PlainText text -> string text
    Wildcard _ -> T.pack <$> f
      where
        remainingInput = lookAhead (dslToParser next)
        -- keep consuming input until the rest of the parsers in line succeed
        f = manyTill anySingle (remainingInput)
    _ -> ""

dslToParser :: [DSL] -> Consumer
dslToParser [] = mempty
dslToParser (x : xs) = do
  result <- try (fromDsl xs x)
  (result :) <$> dslToParser xs

parseDsl :: [DSL] -> T.Text -> Either (ParseErrorBundle T.Text Void) [ParsedMessage]
parseDsl dsl = parse (dslToParser dsl) ""
