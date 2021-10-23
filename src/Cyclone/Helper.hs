module Cyclone.Helper
  ( eitherToMaybe,
    flattenMonoid,
    quote,
    bold,
    codeblock,
  )
where

import qualified Data.Text as T

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Right a) = return a
eitherToMaybe (Left _) = Nothing

flattenMonoid :: Monoid m => Maybe m -> m
flattenMonoid Nothing = mempty
flattenMonoid (Just a) = a

surround :: T.Text -> T.Text -> T.Text
surround modifier text = modifier <> text <> modifier

quote :: T.Text -> T.Text
quote = surround "`"

bold :: T.Text -> T.Text
bold = surround "**"

codeblock :: T.Text -> T.Text -> T.Text
codeblock lang code = "```" <> lang <> "\n" <> code <> "\n```"
