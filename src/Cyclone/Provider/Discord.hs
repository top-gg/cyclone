module Cyclone.Provider.Discord (runDiscordBot, DiscordContext (..)) where

import Control.Monad
import Control.Monad.IO.Class
import Cyclone.Helper
import Cyclone.Parsing.DSL (parseDsl)
import Cyclone.Parsing.Data
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

data MatchingMessage = MatchingMessage
  { matchedConfig :: BotConfig,
    matchedDetection :: Detection,
    matchedVariables :: [EmbeddedVariable]
  }

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
    Just MatchingMessage {matchedDetection, matchedConfig, matchedVariables} -> do
      let detectionName = name (matchedDetection :: Detection)
      let botName = name (matchedConfig :: BotConfig)
      liftIO $ print matchedConfig
      let detectionString = prettyPrintMatcher (matcher matchedDetection)
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
                flattenMonoid (buildVariableList matchedVariables),
                flattenMonoid (buildSourceLink <$> link matchedConfig),
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
buildDeclineLink botId botName =
  "**Decline Link:** <https://top.gg/moderation/decline?id=" <> T.pack (show botId) <> "&" <> "reason=clone--" <> botName <> ">\n"

buildVariableList :: [EmbeddedVariable] -> Maybe T.Text
buildVariableList [] = Nothing
buildVariableList vars =
  return . mconcat $
    [ "**Captured Wildcards (",
      T.pack (show $ length vars),
      ")**\n",
      codeblock "yaml" . T.intercalate "\n" $ map line vars,
      "\n"
    ]
  where
    line (key, value) = key <> ": " <> value

-- | Checking if a message matches a yaml detection
findClone :: Message -> [BotConfig] -> Maybe MatchingMessage
findClone _ [] = Nothing
findClone m (config : configs) =
  case matchConfig m config of
    Nothing -> findClone m configs
    Just (matchedDetection, matchedVariables) ->
      return
        MatchingMessage {matchedConfig = config, matchedVariables, matchedDetection}

matchConfig :: Message -> BotConfig -> Maybe (Detection, [EmbeddedVariable])
matchConfig m BotConfig {detections} =
  let predicate detection = (detection,) <$> matchesMessage m (matcher detection)
   in findMap predicate detections

-- | Whether a message matches either an embed or a text content.
-- | All embed fields must match in order to be considered fully matching.
matchesMessage :: Message -> Matcher -> Maybe [EmbeddedVariable]
matchesMessage m matcher =
  case (messageEmbeds m, messageText m, matcher) of
    -- Matching all fields of an embed, text content is ignored if an embed is present
    -- since embed and content matching are mutually exclusive configurations
    (embeds, _, EmbedMatcher {..}) ->
      let maybeVariables = findMap (\embed -> findMap (isEmbedFieldMatching embed) checkedEmbedFields) embeds
       in extractVariables <$> maybeVariables
      where
        -- TODO: match on all embed fields
        checkedEmbedFields = [\e -> (description, embedDescription e)]
        isEmbedFieldMatching :: Embed -> (Embed -> (Maybe T.Text, Maybe T.Text)) -> Maybe [ParsedMessage]
        isEmbedFieldMatching embed fetcher = uncurry runEmbedField (fetcher embed)
    (_, text, MessageMatcher {..}) -> do
      a <- eitherToMaybe $ parseDsl content
      -- if the parse result is `Right [ParsedMessage]` it must have succeeded parsing
      result <- eitherToMaybe $ parseMessageContent a text
      return $ extractVariables result
  where
    -- Runs one parser against one field field of the embed
    runEmbedField :: Maybe T.Text -> Maybe T.Text -> Maybe [ParsedMessage]
    runEmbedField rawDsl f = do
      -- TODO: don't silently ignore errors converting a raw string to a DSL?
      -- TODO: create parsers once on startup instead of recreating them for every embed check
      parser <- eitherToMaybe . parseDsl =<< rawDsl
      embedField <- f
      eitherToMaybe $ parseMessageContent parser embedField
