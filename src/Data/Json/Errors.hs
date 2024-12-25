module Data.Json.Errors (JsonError (..)) where

data JsonError
  = JsonParseError String
  | ConversionError
  | MissingFieldError String
  deriving (Show, Eq)
