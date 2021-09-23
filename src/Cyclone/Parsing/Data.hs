module Cyclone.Parsing.Data where

import Data.List
import Data.Semigroup (Semigroup)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec (Parsec)
import Text.Megaparsec.Char

type Parser = Parsec Void T.Text

data DSL
  = Emoji {label :: Maybe T.Text} -- {emojiLabel: emoji}
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
    Loop sections -> "Loop: [" <> intercalate ", " (map show sections) <> "]"

data Emoji
  = Native T.Text
  | DiscordFormat T.Text
  deriving (Show)

data ParsedMessage = ParsedMessage {dsl :: DSL, message :: T.Text} deriving (Show, Eq)
