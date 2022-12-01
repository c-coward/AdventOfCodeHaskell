-- | Useful parsing functions
-- Re-export the parsing module to help cut back on extra imports across the project
module Util.Parsing
    ( module Util.Parsing
    , module Data.Attoparsec.Text
    ) where

import Data.Attoparsec.Text

-- | Parse before a newline character
line :: Parser a -> Parser a
line p = p <* endOfLine

-- | Execute many parsers separated by newlines
lines :: Parser a -> Parser [a]
lines p = many' (line p)

-- | Execute a parser surrounded by whitespace
token :: Parser a -> Parser a
token p = skipSpace *> p <* skipSpace