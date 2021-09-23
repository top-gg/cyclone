module Pattie.Parsing.Parser where

import qualified Data.Text as T
import Data.Void
import Pattie.Parsing.DSL
import Pattie.Parsing.Data (DSL (..), Parser)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Debug (dbg)

type Consumer = Parser T.Text

emojiDelimiter :: Parser Char
emojiDelimiter = char ':'

emoji :: Consumer
emoji = between emojiDelimiter emojiDelimiter (takeWhileP Nothing (/= ':'))

fromDsl :: [DSL] -> DSL -> Consumer
fromDsl next = \case
  Emoji -> emoji
  PlainText text -> string text
  -- keep consuming input until the next matching input is found
  Wildcard -> do
    let nextParser = dslToParser next
    a <- manyTill anySingle (lookAhead nextParser)
    return $ T.pack a

dslToParser :: [DSL] -> Consumer
dslToParser [] = mempty
dslToParser (x : xs) = try $ fromDsl xs x <> dslToParser xs
