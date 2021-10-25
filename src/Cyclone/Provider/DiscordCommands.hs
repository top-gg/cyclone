module Cyclone.Provider.DiscordCommands where

import Control.Monad
import Control.Monad.IO.Class
import Cyclone.Helper (codeblock)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.TH (defaultOptions)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TE
import Discord
import Discord.Requests (MessageDetailedOpts (messageDetailedReference), messageDetailedContent)
import qualified Discord.Requests as R
import Discord.Types

-- | Checks to see if the message being replied to is a valid response
commandCheckReplyMessage :: Message -> DiscordHandler ()
commandCheckReplyMessage m = do
  let channelId = messageChannel m
  case referencedMessage m of
    Nothing -> pure ()
    Just referenced -> do
      let getMessage = R.GetChannelMessage (channelId, messageId referenced)
      maybeMessage <- restCall getMessage
      case maybeMessage of
        Left err -> liftIO $ print err
        Right message -> do
          let embeds = messageEmbeds message
          unless (null embeds) do
            liftIO $ print $ head embeds
            let embedString = TL.toStrict . TE.decodeUtf8 $ encodePretty (head embeds)
            let req =
                  R.CreateMessageDetailed channelId $
                    def
                      { messageDetailedReference = Just $ def {referenceMessageId = Just $ messageId m},
                        messageDetailedContent = codeblock "json" embedString
                      }
            _ <- restCall req
            pure ()

runCommands :: Message -> DiscordHandler ()
runCommands m = do
  when ("~check" `T.isPrefixOf` messageText m) do
    commandCheckReplyMessage m
