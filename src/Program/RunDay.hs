{-# LANGUAGE LambdaCase #-}
-- | Common interface for running specific days
module Program.RunDay where

import Data.Attoparsec.Text ( parseOnly, Parser )
import Data.Text ( pack )

-- | Given an input file name for a problem, solve and it print the answers to the output
type Day = String -> IO ()

-- | Run both parts of a day
runDay :: (Show i, Show a, Show b) => Parser i -> (i -> a) -> (i -> b) -> Day
runDay inputParser partA partB inputFile = do
    input <- readFile inputFile >>= (\case
        Left e -> error "Unable to parse"
        Right i -> return i) . parseOnly inputParser . pack

    print $ partA input
    print $ partB input