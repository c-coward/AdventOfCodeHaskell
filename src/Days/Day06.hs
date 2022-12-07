module Days.Day06 where

import Util.Util
import Util.Parsing ( Parser )
import qualified Util.Parsing as P

import Data.List

import qualified Program.RunDay as R (runDay, Day)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

type Input = String
type OutputA = Int
type OutputB = Int

inputParser :: Parser Input
inputParser = P.getLineS

partA :: Input -> OutputA
partA = findStart 4

partB :: Input -> OutputB
partB = findStart 14

findStart :: Int -> String -> Int
findStart n = (+ n) . length . takeWhile ((/=) <*> nub) . map (take n) . tails