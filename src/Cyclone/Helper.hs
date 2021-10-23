module Cyclone.Helper (eitherToMaybe) where

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Right a) = return a
eitherToMaybe (Left _) = Nothing
