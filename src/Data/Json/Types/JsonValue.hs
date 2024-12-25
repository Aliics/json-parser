module Data.Json.Types.JsonValue (JsonValue (..), jsonString, jsonBool, jsonNumber, jsonArray) where

import Data.Json.Errors (JsonError (ConversionError))

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
