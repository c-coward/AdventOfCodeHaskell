-- | Useful parsing functions
module Util.Parsing where

import Data.Attoparsec.Text

-- | Parse before a newline character
lineP :: Parser a -> Parser a
lineP p = p <* endOfLine

-- | Execute many parsers separated by newlines
linesP :: Parser a -> Parser [a]
linesP p = many' $ lineP p

-- | Execute a parser surrounded by whitespace
tokenP :: Parser a -> Parser a
tokenP p = skipSpace *> p <* skipSpace