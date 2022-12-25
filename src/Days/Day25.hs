module Days.Day25 where

import Util.Util
import Util.Parsing ( Parser )
import qualified Util.Parsing as P

import Data.Char ( digitToInt, intToDigit )
import Data.List ( unfoldr )

import qualified Program.RunDay as R (runDay, Day)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

type Input = [String]
type OutputA = String
type OutputB = String

inputParser :: Parser Input
inputParser = P.asString lines

partA :: Input -> OutputA
partA = map parseBQuin .> sum .> decToQuin

partB :: Input -> OutputB
partB = const "merry advent of codemas :)"

parseBQuin = foldl (\b a -> f a + b * 5) 0 where
    f = \case
        '=' -> -2
        '-' -> -1
        c -> digitToInt c

decToQuin = unfoldr getDig .> (\case {[] -> "0"; x -> reverse x}) where
    getDig = do
        (m, r) <- (`mod` 5) .> \case
            3 -> ('=', 5)
            4 -> ('-', 5)
            n -> (intToDigit n, 0)
        \case
            0 -> Nothing
            n -> Just (m, div (r + n) 5)