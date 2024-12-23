module Data.Json where

import Data.Maybe (fromMaybe)
import Text.Parsec.Char (char, digit, newline, spaces, string)
import Text.Parsec.Combinator (between)
import Text.Parsec.String (Parser)
import Text.ParserCombinators.Parsec (many, many1, noneOf, optionMaybe, sepBy, (<|>))

data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonNumber Float
  | JsonString String
  | JsonArray [JsonValue]
  | JsonObject [(String, JsonValue)]
  deriving (Show, Eq)

jsonValueParser :: Parser JsonValue
jsonValueParser = jsonPrimiviteValueParser <|> arrayParser <|> objectParser

jsonPrimiviteValueParser :: Parser JsonValue
jsonPrimiviteValueParser = literalParser <|> stringParser <|> numberParser

literalParser :: Parser JsonValue
literalParser =
  (string "null" >> return JsonNull)
    <|> (string "true" >> return (JsonBool True))
    <|> (string "false" >> return (JsonBool False))

stringParser :: Parser JsonValue
stringParser = JsonString <$> quotedString

numberParser :: Parser JsonValue
numberParser = do
  negative <- optionMaybe (string "-")
  whole <- many1 digit
  decimal <- optionMaybe $ char '.' *> many1 digit

  let sign = fromMaybe "" negative
      frac = maybe "" ("." <>) decimal

  return . JsonNumber . read $ sign <> whole <> frac

arrayParser :: Parser JsonValue
arrayParser = JsonArray <$> between (char '[') (char ']') (jsonPrimiviteValueParser `sepBy` commaDelimiter)

objectParser :: Parser JsonValue
objectParser = JsonObject <$> between (char '{') (char '}') (fieldParser `sepBy` commaDelimiter)
  where
    fieldParser = (,) <$> quotedString <*> (char ':' *> spaces *> jsonValueParser)

quotedString :: Parser String
quotedString = between (char '"') (char '"') nonEscapeChars
  where
    nonEscapeChars = many $ noneOf ['"'] <|> newline

commaDelimiter :: Parser ()
commaDelimiter = char ',' *> spaces <* optionMaybe newline
