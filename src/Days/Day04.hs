module Days.Day04 where

import Data.Attoparsec.Text
import Util.Util
import Util.Parsing ( Parser )
import qualified Util.Parsing as P

import Control.Arrow ((&&&))
import Data.Ix (inRange)
import Data.Bifunctor (bimap)
import Data.Tuple (swap)

import qualified Program.RunDay as R (runDay, Day)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

type Input = [((Int, Int), (Int, Int))]
type OutputA = Int
type OutputB = Int

inputParser :: Parser Input
inputParser = P.lines $ do
    a <- P.decimal
    b <- P.char '-' >> P.decimal
    c <- P.char ',' >> P.decimal
    d <- P.char '-' >> P.decimal
    return ((a, b), (c, d))

partA :: Input -> OutputA
partA = length . filter (uncurry (||) . (within &&& (swap .> within)))

partB :: Input -> OutputB
partB = length . filter (uncurry (||) . (overlap &&& (swap .> overlap)))

testBounds :: ((Int, Int), (Int, Int)) -> (Bool, Bool)
testBounds = uncurry ((inRange &&& inRange) .> uncurry bimap)
within = testBounds .> uncurry (&&)
overlap = testBounds .> uncurry (||)