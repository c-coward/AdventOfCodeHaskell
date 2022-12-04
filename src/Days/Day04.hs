module Days.Day04 where

import Data.Attoparsec.Text
import Util.Util
import Util.Parsing ( Parser )
import qualified Util.Parsing as P

import Control.Arrow ((&&&))
import Data.Ix (inRange)

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
partA = length . filter (uncurry (||) . (uncurry within &&& uncurry (flip within)))

partB :: Input -> OutputB
partB = length . filter (uncurry (||) . (uncurry overlap &&& uncurry (flip overlap)))

within :: (Int, Int) -> (Int, Int) -> Bool
within (a, b) (c, d) = a >= c && b <= d

overlap :: (Int, Int) -> (Int, Int) -> Bool
overlap (a, b) (c, d) = inRange (a, b) c || inRange (a, b) d