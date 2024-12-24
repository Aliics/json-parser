module Data.Json.Types.Json (Json (..), fromJsonValue, lookupJson) where

import Data.Foldable (find)
import Data.Json.Types.JsonValue (JsonValue (..))

newtype Json = Json [(String, JsonValue)] deriving (Show, Eq)

fromJsonValue :: JsonValue -> Json
fromJsonValue (JsonObject x) = Json x
fromJsonValue v = error $ mconcat ["fromJsonValue: ", "invalid Json type ", show v]

lookupJson :: String -> Json -> Maybe JsonValue
lookupJson s (Json xs) = snd <$> find ((== s) . fst) xs
