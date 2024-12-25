module Data.Json
  ( Json (..),
    JsonValue (..),
    FromJson,
    jsonBool,
    jsonNumber,
    jsonString,
    jsonArray,
  )
where

import Data.Json.Internal.Parser (jsonValueParser)
import Data.Json.Types
  ( FromJson (fromJson),
    Json (Json),
    JsonValue (..),
    fromJsonValue,
    jsonArray,
    jsonBool,
    jsonNumber,
    jsonString,
  )
import Text.Parsec (parse)

serialize :: (FromJson a) => String -> Maybe a
serialize s = do
  jsonValue <- case parse jsonValueParser "" s of
    Right v -> Just v
    Left _ -> Nothing
  let json = fromJsonValue jsonValue
  fromJson json
