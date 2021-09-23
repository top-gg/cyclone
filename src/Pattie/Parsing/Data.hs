module Pattie.Parsing.Data where

import Data.Text
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void Text

data DSL
  = Emoji
  | PlainText T.Text
  | Wildcard

instance Show DSL where
  show = \case
    Emoji -> "Emoji"
    PlainText text -> T.unpack $ "PlainText: \"" <> text <> "\""
    Wildcard -> "?"

data Emoji
  = Native T.Text
  | DiscordFormat T.Text
