module Data.Json.Types.JsonValue
  ( JsonValue (..),
    jsonString,
    jsonBool,
    jsonNumber,
    jsonArray,
    jsonValueToString,
  )
where

import Data.Json.Errors (JsonError (ConversionError))
import Data.List (intercalate)

data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonNumber Float
  | JsonString String
  | JsonArray [JsonValue]
  | JsonObject [(String, JsonValue)]
  deriving (Show, Eq)

jsonBool :: JsonValue -> Either JsonError Bool
jsonBool (JsonBool b) = Right b
jsonBool _ = Left ConversionError

jsonNumber :: JsonValue -> Either JsonError Float
jsonNumber (JsonNumber n) = Right n
jsonNumber _ = Left ConversionError

jsonString :: JsonValue -> Either JsonError String
jsonString (JsonString s) = Right s
jsonString _ = Left ConversionError

jsonArray :: JsonValue -> Either JsonError [JsonValue]
jsonArray (JsonArray a) = Right a
jsonArray _ = Left ConversionError

jsonValueToString :: JsonValue -> String
jsonValueToString JsonNull = "null"
jsonValueToString (JsonBool b) = if b then "true" else "false"
jsonValueToString (JsonNumber n) = show n
jsonValueToString (JsonString s) = show s
jsonValueToString (JsonArray xs) = mconcat ["[", intercalate "," (jsonValueToString <$> xs), "]"]
jsonValueToString (JsonObject xs) =
  mconcat ["{", intercalate "," (fieldPair <$> xs), "}"]
  where
    fieldPair (k, v) = mconcat ["\"", k, "\":", jsonValueToString v]
