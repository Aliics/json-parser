module Data.Json.Types
  ( FromJson (..),
    Json (..),
    fromJsonValue,
    lookupJson,
    JsonValue (..),
    jsonBool,
    jsonNumber,
    jsonString,
    jsonArray,
  )
where

import Data.Json.Types.Json (Json (..), fromJsonValue, lookupJson)
import Data.Json.Types.JsonValue (JsonValue (..), jsonArray, jsonBool, jsonNumber, jsonString)

class FromJson a where
  fromJson :: Json -> Maybe a
