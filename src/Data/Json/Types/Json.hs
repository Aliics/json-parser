module Data.Json.Types.Json (Json (..), fromJsonValue, lookupJson, lookupJson') where

import Data.Foldable (find)
import Data.Json.Errors (JsonError (ConversionError, MissingFieldError))
import Data.Json.Types.JsonValue (JsonValue (..))

newtype Json = Json [(String, JsonValue)] deriving (Show, Eq)

fromJsonValue :: JsonValue -> Either JsonError Json
fromJsonValue (JsonObject x) = Right $ Json x
fromJsonValue _ = Left ConversionError

lookupJson' :: String -> Json -> Maybe JsonValue
lookupJson' s (Json xs) = snd <$> find ((== s) . fst) xs

lookupJson :: String -> Json -> Either JsonError JsonValue
lookupJson s v = maybe (Left $ MissingFieldError s) Right (lookupJson' s v)
