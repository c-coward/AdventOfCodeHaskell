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
partA = (+ 4) . length . takeWhile ((/=) <*> nub) . map (take 4) . tails

partB :: Input -> OutputB
partB = (+ 14) . length . takeWhile ((/=) <*> nub) . map (take 14) . tails