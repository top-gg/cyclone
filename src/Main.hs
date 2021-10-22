module Main where

import Control.Exception (try)
import Cyclone.Provider.Discord
import qualified Data.Text as T
import System.Environment (getEnv)
import System.Exit (exitFailure)

getEnvSafe :: T.Text -> IO (Maybe T.Text)
getEnvSafe e = do
  result <- try (T.pack <$> getEnv (T.unpack e)) :: IO (Either IOError T.Text)
  return $ case result of
    Left _ -> Nothing
    Right txt -> Just txt

main :: IO ()
main = do
  maybeToken <- getEnvSafe "CYCLONE_TOKEN"
  case maybeToken of
    Nothing -> putStrLn "Missing environment variable 'CYCLONE_TOKEN'" >> exitFailure
    Just txt -> runDiscordBot txt
