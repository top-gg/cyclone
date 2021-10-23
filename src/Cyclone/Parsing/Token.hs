module Cyclone.Parsing.Token where

import Cyclone.Parsing.Data (Parser)
import Text.Megaparsec
import Text.Megaparsec.Char

colon :: Parser Char
colon = char ':' <?> "colon (':')"

leftBracket :: Parser Char
leftBracket = char '{' <?> "Left Bracket ('{')"

rightBracket :: Parser Char
rightBracket = char '}' <?> "Right Bracket ('}')"

-- TODO: This doesn't work lol unicode is a lie
isUnicodeEmoji :: Char -> Bool
isUnicodeEmoji c = c >= '\x1F000' && c <= '\x1F9FF'

skipSpaces :: Parser p -> Parser p
skipSpaces input = do
  let consume = optional space
  -- Attempt to run the parser first before consuming whitespace
  maybeSuccess <- optional (try input)
  case maybeSuccess of
    -- there might still be more whitespace afterwards
    Just result -> consume *> (result <$ consume)
    Nothing -> consume *> input <* consume
