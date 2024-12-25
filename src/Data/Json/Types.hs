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
import Data.Json.Errors (JsonError)

class FromJson a where
  fromJson :: Json -> Either JsonError a
