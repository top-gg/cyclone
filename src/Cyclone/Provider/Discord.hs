module Cyclone.Provider.Discord (runDiscordBot) where

import Control.Monad.IO.Class
import Cyclone.Parsing.DSL (parseDsl)
import Cyclone.Parsing.Data
import Cyclone.Parsing.Parser
import Data.Either (fromRight)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Discord
import Discord.Requests (MessageDetailedOpts (messageDetailedReference), messageDetailedContent)
import qualified Discord.Requests as R
import Discord.Types hiding (Emoji)

messagePattern :: T.Text
messagePattern =
  "Help:\n\
  \@loop\n\
  \{emoji} **{category: ?}** {emoji}\n\
  \{?}\n\
  \@loop"

messagePatternDsl :: [DSL]
messagePatternDsl = fromRight (error "invalid pattern") $ parseDsl messagePattern

runDiscordBot :: T.Text -> IO ()
runDiscordBot token = do
  TIO.putStrLn "Launching discord bot..."
  print messagePatternDsl
  userFacingError <-
    runDiscord $
      def
        { discordToken = token,
          discordOnEvent = eventHandler
        }
  TIO.putStrLn userFacingError

eventHandler :: Event -> DiscordHandler ()
eventHandler event = case event of
  MessageCreate m -> do
    liftIO $ TIO.putStrLn (messageText m)
    case findClone m of
      Nothing -> pure ()
      Just _ -> do
        let channelId = messageChannel m
        let reply =
              R.CreateMessageDetailed channelId $
                def
                  { messageDetailedReference = Just $ def {referenceMessageId = Just $ messageId m},
                    messageDetailedContent = "This message matches the detection `Help` of `Shitty bot`:\n```" <> messagePattern <> "\n```"
                  }
        _ <- restCall reply
        pure ()
  _ -> pure ()

findClone :: Message -> Maybe [ParsedMessage]
findClone message = either (const Nothing) Just parts
  where
    builtPattern = fromRight [] $ parseDsl messagePattern
    parts = parseMessageContent builtPattern $ messageText message
