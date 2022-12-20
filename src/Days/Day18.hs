module Days.Day18 where

import Util.Util
import Util.Parsing ( Parser )
import qualified Util.Parsing as P

import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad.State

import qualified Program.RunDay as R (runDay, Day)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

type Input = [(Int, Int, Int)]
type OutputA = Int
type OutputB = ()

inputParser :: Parser Input
inputParser = P.lines parseLine

partA :: Input -> OutputA
partA = (map =<< exposed . S.fromList) .> concat .> length

partB :: Input -> OutputB
partB = undefined

allPoints3D :: [(Int, Int, Int)]
allPoints3D = [(x, y, z) | d <- [0..]
    , x <- [0..d], y <- [0..d], let z = d - x - y, z >= 0]

exposed :: S.Set (Int, Int, Int) -> (Int, Int, Int) -> [(Int, Int, Int)]
exposed s (x,y,z) = filter (`S.notMember` s)
    $ concat [[(x+d,y,z), (x,y+d,z), (x,y,z+d)] | d <- [-1,1]]

parseLine = (,,) <$> P.decimal <*> (P.anyChar >> P.decimal) <*> (P.anyChar >> P.decimal)