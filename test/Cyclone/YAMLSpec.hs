module Cyclone.YAMLSpec where

import Control.Exception (evaluate)
import Cyclone.Provider.YAML
import Data.Either
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec = describe "YAML Parser" $ do
  it "parses a default configuration" $ do
    let description =
          "Commands:\n\
          \@loop\n\
          \{emoji} **{category: ?}** {emoji}\n\
          \@loop\n"
    file <- readConfigFile "./test/fixtures/detections/testDetection.yaml"
    let detection =
          Detection
            { name = "Identical help command",
              input = Just "{prefix}help",
              matcher =
                EmbedMatcher
                  { title = Just "Welcome to my help menu, here are my commands",
                    description = Just description,
                    footer = Nothing,
                    fields = Nothing
                  },
              defaults = Nothing
            }
    let result = BotConfig {name = "Jisoo Bot", platform = Nothing, link = Nothing, detections = [detection]}
    print file
    fromRight (error "oops") file `shouldBe` result
