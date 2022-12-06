module Days.Day06 where

import Util.Util
import Util.Parsing ( Parser )
import qualified Util.Parsing as P

import Data.List (nub)

import qualified Program.RunDay as R (runDay, Day)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

type Input = String
type OutputA = Int
type OutputB = Int

inputParser :: Parser Input
inputParser = P.getLineS

partA :: Input -> OutputA
partA = locateDistinct 4

partB :: Input -> OutputB
partB = locateDistinct 14

locateDistinct :: Int -> String -> Int
locateDistinct n xs = case splitAt n xs of
    (y, ys) | length (nub y) == n -> n
    _ -> 1 + locateDistinct n (tail xs)