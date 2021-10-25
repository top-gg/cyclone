module Cyclone.Parsing.Data where

import Data.List (intercalate)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec (Parsec)

type Parser = Parsec Void T.Text

data LabeledVariable
  = Emoji
  | Wildcard
  | List {delimiter :: T.Text, pattern :: T.Text}
  deriving (Eq)

type EmbeddedVariable = (T.Text, T.Text)

data DSL
  = PlainText T.Text
  | Loop [DSL]
  | Labeled LabeledVariable (Maybe T.Text)
  deriving (Eq)

showLabeled :: Maybe T.Text -> String -> String
showLabeled Nothing a = a
showLabeled (Just label) a = T.unpack label <> ": " <> a

instance Show DSL where
  show = \case
    Labeled Emoji e -> showLabeled e "Emoji"
    Labeled Wildcard w -> showLabeled w "?"
    -- TODO: show list contents?
    Labeled (List _ _) w -> showLabeled w "List [" <> "]"
    PlainText text -> T.unpack $ "PlainText: \"" <> T.replace "\n" "<newline>" text <> "\""
    Loop sections -> "Loop: [" <> intercalate ", " (map show sections) <> "]"

data Emoji
  = Native T.Text
  | DiscordFormat T.Text
  deriving (Show)

data ParsedMessage = ParsedMessage {dsl :: DSL, message :: T.Text}
  deriving (Show, Eq)
