module Cyclone.Provider.Discord (runDiscordBot, DiscordContext (..)) where

import Control.Monad
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
onMessageCreate configs message = do
  let user = messageAuthor message
  when (userIsBot user) $ do
    checkForClones configs message

checkForClones :: [BotConfig] -> Message -> DiscordHandler ()
checkForClones configs m =
  case findClone m configs of
    Nothing -> pure ()
    Just (config, detection) -> do
      let detectionName = name (detection :: Detection)
      let botName = name (config :: BotConfig)
      liftIO $ print config
      let detectionString = prettyPrintMatcher (matcher detection)
      let author = messageAuthor m
      let channelId = messageChannel m
      let content =
            mconcat
              [ "This message matched the detection for ",
                quote detectionName,
                " of the bot ",
                quote botName,
                "\n",
                codeblock "yaml" detectionString,
                "\n",
                flattenMonoid (buildSourceLink <$> link config),
                buildDeclineLink (userId author) botName
              ]
      let reply =
            R.CreateMessageDetailed channelId $
              def
                { messageDetailedReference = Just $ def {referenceMessageId = Just $ messageId m},
                  messageDetailedContent = content
                }
      _ <- restCall reply
      pure ()

buildSourceLink :: T.Text -> T.Text
buildSourceLink link = "**Source Link:** <" <> link <> ">\n"

buildDeclineLink :: Snowflake -> T.Text -> T.Text
buildDeclineLink botId botName = "**Decline Link:** <https://top.gg/moderation/decline?id=" <> T.pack (show botId) <> "&" <> "reason=clone--" <> botName <> ">\n"

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
    (embeds, _, EmbedMatcher {..}) ->
      any (\embed -> all (isEmbedFieldMatching embed) checkedEmbedFields) embeds
      where
        -- TODO: match on all embed fields
        checkedEmbedFields = [\e -> (description, embedDescription e)]
        isEmbedFieldMatching :: Embed -> (Embed -> (Maybe T.Text, Maybe T.Text)) -> Bool
        isEmbedFieldMatching embed fetcher = uncurry runEmbedField (fetcher embed)
    (_, text, MessageMatcher {..}) -> isJust $ do
      a <- eitherToMaybe $ parseDsl content
      -- if the parse result is `Right [ParsedMessage]` it must have succeeded parsing
      _ <- eitherToMaybe $ parseMessageContent a text
      return True
  where
    -- Runs one parser against one field field of the embed
    runEmbedField :: Maybe T.Text -> Maybe T.Text -> Bool
    runEmbedField rawDsl f = isJust $ do
      -- TODO: don't silently ignore errors converting a raw string to a DSL?
      -- TODO: create parsers once on startup instead of recreating them for every embed check
      parser <- eitherToMaybe . parseDsl =<< rawDsl
      embedField <- f
      eitherToMaybe $ parseMessageContent parser embedField
