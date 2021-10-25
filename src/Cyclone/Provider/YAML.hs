{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Cyclone.Provider.YAML
  ( readConfigFile,
    getBotConfigurations,
    prettyPrintMatcher,
    BotConfig (..),
    Platform (..),
    Detection (..),
    Matcher (..),
    DefaultTypes (..),
    DefaultDeclaredVariables,
  )
where

import Data.Aeson.TH
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Yaml
import GHC.Generics (Generic)
import System.FilePath
import UnliftIO.Directory

data Platform = Discord deriving (Generic, Eq)

instance Show Platform where
  show = \case
    Discord -> "discord"

data BotConfig = BotConfig
  { name :: T.Text,
    platform :: Maybe Platform,
    link :: Maybe T.Text,
    detections :: [Detection]
  }
  deriving (Show, Generic, Eq)

type DefaultDeclaredVariables = M.Map T.Text DefaultTypes

data Detection = Detection
  { name :: T.Text,
    input :: Maybe T.Text,
    matcher :: Matcher,
    defaults :: Maybe DefaultDeclaredVariables
  }
  deriving (Show, Generic, Eq)

data DefaultTypes
  = Value T.Text
  | Values [T.Text]
  deriving (Show, Generic, Eq)

data FieldMatcher = FieldMatcher
  { title :: Maybe T.Text,
    description :: Maybe T.Text
  }
  deriving (Show, Generic, Eq)

data Matcher
  = EmbedMatcher
      { title :: Maybe T.Text,
        description :: Maybe T.Text,
        footer :: Maybe T.Text,
        fields :: Maybe FieldMatcher
      }
  | MessageMatcher
      { content :: T.Text
      }
  deriving (Show, Generic, Eq)

-- deriving a parser for the yaml data
deriveJSON
  defaultOptions
    { sumEncoding = defaultTaggedObject {tagFieldName = "type"},
      omitNothingFields = True,
      constructorTagModifier = \case
        "EmbedMatcher" -> "embed"
        "MessageMatcher" -> "message"
        _ -> error "invalid tag name"
    }
  ''Matcher

deriveJSON
  defaultOptions
    { sumEncoding = UntaggedValue,
      omitNothingFields = True
    }
  ''DefaultTypes

deriveJSON defaultOptions ''Platform
deriveJSON defaultOptions { omitNothingFields = True } ''FieldMatcher
deriveJSON defaultOptions ''BotConfig
deriveJSON defaultOptions ''Detection

-- | Reads all detections for a single bot from a file
readConfigFile :: FilePath -> IO (Either ParseException BotConfig)
readConfigFile = decodeFileEither

isYamlFile :: T.Text -> Bool
isYamlFile name = any (`T.isSuffixOf` name) yamlSuffixes
  where
    yamlSuffixes = ["yaml", "yml"]

-- | Filepaths containing all detections
allDetections :: IO [FilePath]
allDetections = map ("./detections" </>) . filter (isYamlFile . T.pack) <$> getDirectoryContents "./detections"

-- | Gets the configuration of all bot configuration
-- | Fails on the first incorrectly formatted config
getBotConfigurations :: IO (Either ParseException [BotConfig])
getBotConfigurations = catEithers <$> (traverse readConfigFile =<< allDetections)

catEithers :: [Either a b] -> Either a [b]
catEithers [] = return []
catEithers (x : xs) = do
  existing <- x
  next <- catEithers xs
  return (existing : next)

prettyPrintMatcher :: Matcher -> T.Text
prettyPrintMatcher = decodeUtf8 . encode
