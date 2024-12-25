module Data.Json.Types
  ( FromJson (..),
    ToJson (..),
    Json (..),
    fromJsonValue,
    lookupJson,
    JsonValue (..),
    jsonBool,
    jsonNumber,
    jsonString,
    jsonArray,
    jsonValueToString,
  )
where

import Data.Json.Errors (JsonError)
import Data.Json.Types.Json (Json (..), fromJsonValue, lookupJson)
import Data.Json.Types.JsonValue
  ( JsonValue (..),
    jsonArray,
    jsonBool,
    jsonNumber,
    jsonString,
    jsonValueToString,
  )

class FromJson a where
  fromJson :: Json -> Either JsonError a

class ToJson a where
  toJson :: a -> Json
