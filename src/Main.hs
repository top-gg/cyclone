{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import Data.Void
import Pattie.Parsing.DSL
import Pattie.Parsing.Data
import Pattie.Parsing.Parser
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Debug

createParser :: T.Text -> Either (ParseErrorBundle T.Text Void) Consumer
createParser input = do
  dsl <- parse parseDsl "" input
  return $ dbg "original" $ dslToParser dsl

main :: IO ()
main = do
  let inputText = ":warning: **MODERATION EXAMPLE INPUT WITH SPACES**  :warning:"
  case createParser "{emoji} {?} {emoji}" of
    Left err -> print err
    Right parser -> parseTest parser inputText
