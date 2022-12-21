module Days.Day17 where

import Util.Util
import Util.Parsing ( Parser )
import qualified Util.Parsing as P

import Control.Monad.State
import qualified Data.Set as S

import qualified Program.RunDay as R (runDay, Day)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

type Input = ()
type OutputA = ()
type OutputB = ()

inputParser :: Parser Input
inputParser = undefined

partA :: Input -> OutputA
partA = undefined

partB :: Input -> OutputB
partB = undefined

rockList =
    [ [P 2 0, P 3 0, P 4 0, P 5 0]
    , [P 3 0, P 3 1, P 3 2, P 2 1, P 4 1]
    , [P 2 0, P 3 0, P 4 0, P 4 1, P 4 2]
    , [P 2 0, P 2 1, P 2 2, P 2 3]
    , [P 2 0, P 3 0, P 2 1, P 3 1]]

left = map (subtract $ P 1 0)
right = map (+ P 1 0)

data Sim = Sim
    { rocks :: [[Pair Int]]
    , jets :: String
    , game :: S.Set (Pair Int)}
makeProblem s = Sim (cycle rockList) (cycle s)
    $ S.fromList [P x 0 | x <- [0..6]]
toMoves '<' = left
toMoves '>' = right

simulate :: Int -> State Sim ()
simulate k = forM_ [1..k] $ const runSim

runSim :: State Sim ()
runSim = do
    r <- gets $ rocks .> head
    -- jets' 
    return ()

doubleDown :: [[Pair Int]] -> [[Pair Int]]
doubleDown = concatMap (\a -> [a, map (subtract $ P 0 1) a])

applyMove :: [[Pair Int]] -> String -> [[[Pair Int]]]
applyMove r = map toMoves .> scanl (flip map) r .> zipWith (\y -> map (map (subtract $ P 0 y))) [0..] .> map doubleDown