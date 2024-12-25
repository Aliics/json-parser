module Data.Json
  ( Json (..),
    JsonValue (..),
    FromJson,
    jsonBool,
    jsonNumber,
    jsonString,
    jsonArray,
    serialize,
    lookupJson,
  )
where

import Data.Json.Errors (JsonError (JsonParseError))
import Data.Json.Internal.Parser (jsonValueParser)
import Data.Json.Types
  ( FromJson (fromJson),
    Json (Json),
    JsonValue (..),
    fromJsonValue,
    jsonArray,
    jsonBool,
    jsonNumber,
    jsonString, lookupJson,
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
