module Data.Json.Internal.Parser (jsonValueParser) where

import Data.Functor
import Data.Json.Types.JsonValue (JsonValue (..))
import Data.Maybe
import Text.Parsec.Char (char, digit, spaces, string)
import Text.Parsec.Combinator (between)
import Text.Parsec.String (Parser)
import Text.ParserCombinators.Parsec (many, many1, noneOf, optionMaybe, sepBy, (<|>))

jsonValueParser :: Parser JsonValue
jsonValueParser =
  literalParser
    <|> stringParser
    <|> numberParser
    <|> arrayParser
    <|> objectParser

literalParser :: Parser JsonValue
literalParser =
  (string "null" $> JsonNull)
    <|> (string "true" $> JsonBool True)
    <|> (string "false" $> JsonBool False)

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
arrayParser = JsonArray <$> surroundedBy (char '[') (char ']') (jsonValueParser `sepBy` commaDelimiter)

objectParser :: Parser JsonValue
objectParser = JsonObject <$> surroundedBy (char '{') (char '}') (fieldParser `sepBy` commaDelimiter)
  where
    fieldParser = (,) <$> quotedString <*> (spacesAround (char ':') *> jsonValueParser)

quotedString :: Parser String
quotedString = between (char '"') (char '"') nonEscapeChars
  where
    nonEscapeChars = many $ noneOf ['"', '\n']

surroundedBy :: Parser b -> Parser d -> Parser a -> Parser a
surroundedBy open close = between (spacesAround open) (spacesAround close)

commaDelimiter :: Parser Char
commaDelimiter = spacesAround $ char ','

spacesAround :: Parser a -> Parser a
spacesAround = between spaces spaces
