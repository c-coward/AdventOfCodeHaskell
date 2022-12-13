-- | Useful parsing functions
-- Re-export the parsing module to help cut back on extra imports across the project
module Util.Parsing
    ( module Util.Parsing
    , module Data.Attoparsec.Text -- Avoid re importing parsing funcs
    ) where

import Data.Attoparsec.Text
import Control.Applicative
import Data.Text ( Text, unpack )

import Prelude hiding ( takeWhile, lines, getLine )

-- | Parse a single line as text
getLine :: Parser Text
getLine = takeWhile (not . isEndOfLine)

-- | Parse a single line as a string
getLineS :: Parser String
getLineS = unpack <$> getLine

-- | Parse before a newline character
line :: Parser a -> Parser a
line p = p <* endOfLine

-- | Execute many parsers separated by newlines
lines :: Parser a -> Parser [a]
lines p = many' (line p)

-- | Execute a parser surrounded by whitespace
token :: Parser a -> Parser a
token p = skipSpace *> p <* skipSpace

-- | Helpful function for manipulating the underlying text directly
asText :: (Text -> a) -> Parser a
asText = (<$> takeText)

-- | Converts the underlying text to a string before manipulating it
asString :: (String -> a) -> Parser a
asString p = p . unpack <$> takeText

-- | Parse elements, ignoring a common separator
split :: Parser a -> Parser b -> Parser [a]
split p b = ((:) <$> p <*> many' (b >> p)) <|> return []