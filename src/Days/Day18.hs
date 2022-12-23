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

type Input = S.Set Point3
type OutputA = Int
type OutputB = Int

inputParser :: Parser Input
inputParser = S.fromList <$> P.lines parseLine

partA :: Input -> OutputA
partA = (S.map =<< exposed) .> S.unions .> length

partB :: Input -> OutputB
partB = undefined

type Point3 = (Int, Int, Int)
type PointPair = (Point3, Point3)

data Problem = Problem
    { points :: S.Set PointPair
    , sets :: M.Map PointPair Int
    , num :: Int}
makeProblem s = Problem s M.empty 0

exposed :: S.Set Point3 -> Point3 -> S.Set PointPair
exposed s a@(x,y,z) = S.filter (fst .> (`S.notMember` s)) . S.fromList
    $ concat [[((x+d,y,z), a), ((x,y+d,z), a), ((x,y,z+d), a)] | d <- [-1,1]]

parseLine = (,,) <$> P.decimal <*> (P.anyChar >> P.decimal) <*> (P.anyChar >> P.decimal)