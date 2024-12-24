module Data.Json.Types.JsonValue (JsonValue (..), jsonString, jsonBool, jsonNumber, jsonArray) where

data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonNumber Float
  | JsonString String
  | JsonArray [JsonValue]
  | JsonObject [(String, JsonValue)]
  deriving (Show, Eq)

jsonBool :: JsonValue -> Maybe Bool
jsonBool (JsonBool b) = Just b
jsonBool _ = Nothing

jsonNumber :: JsonValue -> Maybe Float
jsonNumber (JsonNumber n) = Just n
jsonNumber _ = Nothing

jsonString :: JsonValue -> Maybe String
jsonString (JsonString s) = Just s
jsonString _ = Nothing

jsonArray :: JsonValue -> Maybe [JsonValue]
jsonArray (JsonArray a) = Just a
jsonArray _ = Nothing
