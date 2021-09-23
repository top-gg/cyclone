module Pattie.Parsing.Data where

import Data.Semigroup (Semigroup)
import Data.Text
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec (Parsec)
import Text.Megaparsec.Char

type Parser = Parsec Void Text

data DSL
  = Emoji {label :: Maybe T.Text} -- {emoji: emoji}
  | PlainText T.Text
  | Wildcard {label :: Maybe T.Text} -- {category: ?}
  | Loop [DSL]
  deriving (Eq)

showLabeled :: Maybe T.Text -> String -> String
showLabeled Nothing a = a
showLabeled (Just label) a = T.unpack label <> ": " <> a

instance Show DSL where
  show = \case
    Emoji e -> showLabeled e "Emoji"
    Wildcard w -> showLabeled w "?"
    PlainText text -> T.unpack $ "PlainText: \"" <> text <> "\""
    _ -> "??"

data Emoji
  = Native T.Text
  | DiscordFormat T.Text
  deriving (Show)

data ParsedMessage = ParsedMessage {dsl :: DSL, message :: T.Text} deriving (Show, Eq)
