module Cyclone.Parsing.Parser where

import Cyclone.Parsing.Data (DSL (..), ParsedMessage (ParsedMessage), Parser)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

type Consumer = Parser [ParsedMessage]

colon :: Parser Char
colon = char ':' <?> "colon (':')"

notColons :: Parser T.Text
notColons =
  let tokenLabel = Just "not-colon (not ':')"
   in takeWhileP tokenLabel (/= ':')

emoji :: Parser T.Text
emoji = colon *> notColons <* colon

fromDsl :: [DSL] -> DSL -> Parser ParsedMessage
fromDsl next t =
  ParsedMessage t <$> case t of
    Emoji _ -> emoji
    PlainText text -> string text
    Wildcard _ -> T.pack <$> wildcard
      where
        remainingInput = lookAhead (dslToParser next)
        -- keep consuming characters one-by-one until the rest of the parsers in line all succeed
        wildcard = manyTill anySingle remainingInput
    Loop _items -> undefined

dslToParser :: [DSL] -> Consumer
dslToParser [] = mempty
dslToParser (x : xs) = do
  result <- try $ fromDsl xs x
  rest <- dslToParser xs
  return $ result : rest

parseDsl :: [DSL] -> T.Text -> Either (ParseErrorBundle T.Text Void) [ParsedMessage]
parseDsl dsl = parse (dslToParser dsl) ""
