module Days.Day18 where

import Util.Util
import Util.Parsing ( Parser )
import qualified Util.Parsing as P

import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad.State
import Data.List (maximumBy, minimumBy)
import Data.Ord (comparing)
import Data.Ix (inRange)

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

-- partB :: Input -> OutputB
partB = makeProblem .> execState bfs .> surface

type Point3 = (Int, Int, Int)
type PointPair = (Point3, Point3)
px (x,_,_) = x
py (_,y,_) = y
pz (_,_,z) = z
pAdd (a,b,c) (x,y,z) = (a+x,b+y,c+z)

data Problem = Problem
    { lava :: S.Set Point3
    , bounds :: PointPair
    , vis :: S.Set Point3
    , surface :: Int}
makeProblem s = Problem s (box s) S.empty 0

dirs = concat [[(d,0,0), (0,d,0), (0,0,d)] | d <- [-1,1]]
search :: Point3 -> State Problem ()
search p = do
    sinceVis <- gets $ vis .> S.member p
    unless sinceVis $ do
        cond <- gets $ lava .> S.member p
        if cond then modify (\p -> p{surface = surface p + 1}) else do
            modify $ \prob@Problem{..} -> prob{vis = S.insert p vis}
            mapM_ search =<< gets (\Problem{..} ->
                map (pAdd p) dirs |> filter (`S.notMember` vis) |> filter (inRange bounds))

bfs :: State Problem ()
bfs = gets bounds >>= search . snd

box :: S.Set Point3 -> PointPair
box = do
    xl <- px . minimumBy (comparing px)
    xh <- px . maximumBy (comparing px)
    yl <- py . minimumBy (comparing py)
    yh <- py . maximumBy (comparing py)
    zl <- pz . minimumBy (comparing pz)
    zh <- pz . maximumBy (comparing pz)
    return ((xl-1,yl-1,zl-1), (xh+1,yh+1,zh+1))

exposed :: S.Set Point3 -> Point3 -> S.Set PointPair
exposed s a@(x,y,z) = S.filter (fst .> (`S.notMember` s)) . S.fromList
    $ concat [[((x+d,y,z), a), ((x,y+d,z), a), ((x,y,z+d), a)] | d <- [-1,1]]

parseLine = (,,) <$> P.decimal <*> (P.anyChar >> P.decimal) <*> (P.anyChar >> P.decimal)