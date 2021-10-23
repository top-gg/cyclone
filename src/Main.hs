module Main where

import Control.Exception (try)
import Cyclone.Provider.Discord
import Cyclone.Provider.YAML
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
  result <- getBotConfigurations
  case result of
    Left a -> print a >> exitFailure
    Right configs -> do
      putStrLn $ "Starting bot with " <> show (length configs) <> " detection configurations..."
      print result
      case maybeToken of
        Nothing -> putStrLn "Missing environment variable 'CYCLONE_TOKEN'" >> exitFailure
        Just token -> runDiscordBot DiscordContext {token, configs}
