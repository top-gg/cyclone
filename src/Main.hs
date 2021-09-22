{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import Data.Void
import Pattie.Parsing.DSL
import Pattie.Parsing.Data
import Pattie.Parsing.Parser
import Text.Megaparsec
import Text.Megaparsec.Char

createParser :: T.Text -> Either (ParseErrorBundle T.Text Void) Consumer
createParser input = do
  dsl <- parse parseDsl "" input
  return $ dslToParser dsl

main :: IO ()
main = do
  let inputText = ":warning: **MODERATION** :warning:"
  case createParser "{emoji} {?} {emoji}" of
    Left err -> print err
    Right parser -> parseTest parser inputText
