module Data.Json
  ( Json (..),
    JsonValue (..),
    FromJson,
    ToJson,
    fromJsonValue,
    jsonBool,
    jsonNumber,
    jsonString,
    jsonArray,
    serialize,
    deserialize,
    lookupJson,
    jsonValueToString,
  )
where

import Data.Json.Errors (JsonError (JsonParseError))
import Data.Json.Internal.Parser (jsonValueParser)
import Data.Json.Types
  ( FromJson (fromJson),
    Json (Json),
    JsonValue (..),
    ToJson (..),
    fromJsonValue,
    jsonArray,
    jsonBool,
    jsonNumber,
    jsonString,
    jsonValueToString,
    lookupJson,
  )
import Text.Parsec (parse)

serialize :: (FromJson a) => String -> Either JsonError a
serialize s = do
  jsonValue <-
    case parse jsonValueParser "" s of
      Right v -> Right v
      Left e -> Left . JsonParseError $ show e
  json <- fromJsonValue jsonValue
  fromJson json

deserialize :: (ToJson a) => a -> String
deserialize v =
  let (Json xs) = toJson v
   in jsonValueToString $ JsonObject xs
