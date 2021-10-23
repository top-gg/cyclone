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
  )
where

import Data.Aeson.TH
import Data.List (isSuffixOf)
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Yaml
import Debug.Trace (traceIO, traceShowId)
import GHC.Generics (Generic)
import System.FilePath
import UnliftIO.Directory

data Platform = Discord deriving (Generic, Eq)

instance Show Platform where
  show = \case
    Discord -> "discord"

instance FromJSON Platform

data BotConfig = BotConfig
  { name :: T.Text,
    platform :: Maybe Platform,
    link :: Maybe T.Text,
    detections :: [Detection]
  }
  deriving (Show, Generic, Eq)

instance FromJSON BotConfig

data Detection = Detection
  { name :: T.Text,
    input :: Maybe T.Text,
    matcher :: Matcher,
    defaults :: Maybe (M.Map T.Text DefaultTypes)
  }
  deriving (Show, Generic, Eq)

instance FromJSON Detection

data DefaultTypes
  = Value T.Text
  | Values [T.Text]
  deriving (Show, Generic, Eq)

instance FromJSON DefaultTypes

data Matcher
  = EmbedMatcher
      { title :: Maybe T.Text,
        description :: Maybe T.Text,
        footer :: Maybe T.Text
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
