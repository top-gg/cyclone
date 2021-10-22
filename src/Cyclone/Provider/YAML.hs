{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Cyclone.Provider.YAML
  ( readConfigFile,
    BotConfig (..),
    Platform (..),
    Detection (..),
    Matcher (..),
    DefaultTypes (..),
  )
where

import Data.Aeson.TH
import Data.Char (toLower)
import Data.Map
import qualified Data.Text as T
import Data.Yaml
import GHC.Generics (Generic)
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
    defaults :: Maybe (Map T.Text DefaultTypes)
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

matcherType :: T.Text -> T.Text
matcherType "embed" = "embed"
matcherType "message" = "message"
matcherType _ = error "invalid tag type"

-- deriving a parser for the yaml data
deriveJSON
  defaultOptions
    { sumEncoding = defaultTaggedObject {tagFieldName = "type"},
      constructorTagModifier = \case
        "EmbedMatcher" -> "embed"
        "MessageMatcher" -> "message"
        _ -> error "invalid tag name"
    }
  ''Matcher

-- instance FromJSON Matcher where
--   parseJSON = generic

-- | Reads all detections for a single bot from a file
readConfigFile :: FilePath -> IO (Either ParseException BotConfig)
readConfigFile = decodeFileEither

-- | Filepaths containing all detections
allDetections :: IO [FilePath]
allDetections = getDirectoryContents "./detections"

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
