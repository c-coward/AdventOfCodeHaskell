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
type OutputB = ()

inputParser :: Parser Input
inputParser = P.asString lines

partA :: Input -> OutputA
partA = map parseBQuin .> sum .> decToQuin

partB :: Input -> OutputB
partB = undefined

parseBQuin :: String -> Int
parseBQuin = foldl (\b a -> f a + b * 5) 0 where
    f = \case
        '=' -> -2
        '-' -> -1
        c -> digitToInt c

decToQuin = unfoldr getDig .> (\case {[] -> "0"; x -> reverse x}) where
    getDig :: Int -> Maybe (Char, Int)
    getDig = do
        m <- (`mod` 5)
        let (m', r) = case m of
                3 -> ('=', 5)
                4 -> ('-', 5)
                n -> (intToDigit n, 0)
        \case
            0 -> Nothing
            n -> Just (m', div (r + n) 5)