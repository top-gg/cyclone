module Cyclone.Provider.Discord (runDiscordBot, DiscordContext (..)) where

import Control.Monad
import Control.Monad.IO.Class
import Cyclone.Helper
import Cyclone.Parsing.DSL (parseDsl)
import Cyclone.Parsing.Data
import Cyclone.Parsing.Parser
import Cyclone.Provider.YAML
import qualified Data.Map as M
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
    liftIO $ print (messageEmbeds message)
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
                flattenMonoid (buildVariableList matchedVariables (defaults matchedDetection)),
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

-- I'm sorry for the war crimes committed in this function
buildVariableList :: [EmbeddedVariable] -> Maybe DefaultDeclaredVariables -> Maybe T.Text
buildVariableList [] _ = Nothing
buildVariableList vars maybeMap =
  return . mconcat $ case maybeMap of
    Just defaults ->
      [header (Just defaults), variableList (Just defaults), "\n"]
    Nothing ->
      [ header Nothing,
        variableList Nothing,
        "\n"
      ]
  where
    varList = M.fromList vars
    identicalVariables defaults =
      map fst $ filter go (M.toList defaults)
      where
        go (key, Value value) = Just value == M.lookup key varList
        go (_, _) = False
    checkValueMatch (Value expected) input
      | expected == input = "# ⚠️ Identical to source\n"
      | otherwise = "# Source uses: \"" <> expected <> "\"\n"
    checkValueMatch _ _ = ""
    line Nothing (key, value) = mconcat [key, ": ", value]
    line (Just defaults) (key, value) =
      case M.lookup key defaults of
        Nothing -> mconcat [key, ": ", value]
        Just expected -> mconcat [checkValueMatch expected value, key, ": ", value]
    header maybeDefaults =
      case maybeDefaults of
        Nothing -> sharedHeader <> "\n"
        Just defaults ->
          -- intersecting the 2 maps lists to make sure we only take into
          -- account variables declared in both the DSL and the defaults when
          -- calculating the difference between supplied and defaulted variables
          let commons = M.intersection defaults varList
              identicalCount = length . identicalVariables $ commons
              changedCount = length commons - identicalCount
           in if changedCount == 0
                then sharedHeader <> "\n"
                else sharedHeader <> " (`" <> T.pack (show changedCount) <> "` changed)\n"
    sharedHeader = "**Captured Wildcards (" <> T.pack (show $ length vars) <> ")**"
    variableList defaults = codeblock "yaml" . T.intercalate "\n" $ map (line defaults) vars

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
