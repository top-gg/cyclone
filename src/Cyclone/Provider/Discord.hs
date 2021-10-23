module Cyclone.Provider.Discord (runDiscordBot, DiscordContext (..)) where

import Control.Monad.IO.Class
import Cyclone.Helper
import Cyclone.Parsing.DSL (parseDsl)
import Cyclone.Parsing.Parser
import Cyclone.Provider.YAML
import Data.List (find)
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Discord
import Discord.Requests (MessageDetailedOpts (messageDetailedReference), messageDetailedContent)
import qualified Discord.Requests as R
import Discord.Types hiding (Emoji)

data DiscordContext = DiscordContext {token :: T.Text, configs :: [BotConfig]}

runDiscordBot :: DiscordContext -> IO ()
runDiscordBot DiscordContext {..} = do
  TIO.putStrLn "Launching discord bot..."
  userFacingError <-
    runDiscord $
      def
        { discordToken = token,
          discordOnEvent = eventHandler configs
        }
  TIO.putStrLn userFacingError

eventHandler :: [BotConfig] -> Event -> DiscordHandler ()
eventHandler configs = \case
  MessageCreate m -> onMessageCreate configs m
  _ -> pure ()

onMessageCreate :: [BotConfig] -> Message -> DiscordHandler ()
onMessageCreate configs m = do
  liftIO $ TIO.putStrLn (messageText m)
  let embeds = messageEmbeds m
  if not (null embeds)
    then liftIO $ print embeds
    else pure ()
  case findClone m configs of
    Nothing -> pure ()
    Just (config, detection) -> do
      let detectionName = name (detection :: Detection)
      let botName = name (config :: BotConfig)
      liftIO $ print config
      let detectionString = prettyPrintMatcher (matcher detection)
      let channelId = messageChannel m
      let reply =
            R.CreateMessageDetailed channelId $
              def
                { messageDetailedReference =
                    Just $ def {referenceMessageId = Just $ messageId m},
                  messageDetailedContent =
                    "This message matches the detection `" <> detectionName <> "` of `" <> botName <> "`:\n```yaml\n" <> detectionString <> "\n```"
                }
      _ <- restCall reply
      pure ()

-- | Checking if a message matches a yaml detection
findClone :: Message -> [BotConfig] -> Maybe (BotConfig, Detection)
findClone _ [] = Nothing
findClone m (config : configs) =
  case matchConfig m config of
    Nothing -> findClone m configs
    Just detection -> Just (config, detection)

matchConfig :: Message -> BotConfig -> Maybe Detection
matchConfig m BotConfig {detections} = find (matchesMessage m . matcher) detections

-- | Whether a message matches either an embed or a text content.
-- | All embed fields must match in order to be considered fully matching.
matchesMessage :: Message -> Matcher -> Bool
matchesMessage m matcher =
  case (messageEmbeds m, messageText m, matcher) of
    -- Matching all fields of an embed, text content is ignored if an embed is present
    -- since embed and content matching are mutually exclusive configurations
    -- we're pattern matching the array here to force messages without embeds to fallthrough to MessageMatcher
    (x : xs, _, EmbedMatcher {..}) ->
      -- TODO: match on all embed fields
      let fields = [\e -> (description, embedAuthor e >>= embedAuthorName)]
       in any (\e -> runEmbedField description (embedDescription e)) (x : xs)
    (_, text, MessageMatcher {..}) -> isJust $ do
      a <- eitherToMaybe $ parseDsl content
      -- if the parse result is `Right [ParsedMessage]` it must have succeeded parsing
      _ <- eitherToMaybe $ parseMessageContent a text
      return True
    _ -> False
  where
    -- Runs one parser against one field field of the embed
    runEmbedField :: Maybe T.Text -> Maybe T.Text -> Bool
    runEmbedField rawDsl f = isJust $ do
      -- TODO: don't silently ignore errors converting a raw string to a DSL?
      parser <- eitherToMaybe . parseDsl =<< rawDsl
      embedField <- f
      eitherToMaybe $ parseMessageContent parser embedField
