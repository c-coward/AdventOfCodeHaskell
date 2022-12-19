module Days.Day16 where

import Util.Util
import Util.Parsing ( Parser )
import qualified Util.Parsing as P
import Data.Text ( Text )

import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State

import qualified Program.RunDay as R (runDay, Day)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

type Input = [(Text, Int, [Text])]
type OutputA = Int
type OutputB = Int

inputParser :: Parser Input
inputParser = P.lines parseLine

partA :: Input -> OutputA
partA = makeProblem .> search "AA" 30 .> M.elems .> maximum

partB :: Input -> Int
partB = makeProblem .> search "AA" 26 .> disjointMaximum

disjointMaximum :: M.Map (S.Set Text) Int -> Int
disjointMaximum m = maximum [ m M.! i + m M.! j
    | i <- M.keys m, j <- M.keys m
    , S.disjoint i j]

makeProblem :: Input -> Problem
makeProblem xs = Problem graph rates where
    graph = floydWarshall $ foldr (\(t,_,ts) -> M.insert t ts) M.empty xs
    rates = foldr (\(t,i,_) -> M.insert t i) M.empty xs

floydWarshall :: M.Map Text [Text] -> M.Map (Text, Text) Int
floydWarshall graph = foldr update dists pairs where
    keys = M.keys graph
    dists = M.fromList [((i, j), edge i j) | i <- keys, j <- keys]
    pairs = [(i, j, k) | k <- keys, j <- keys, i <- keys]
    edge i j | i == j = 0
             | j `elem` (graph M.! i) = 1
             | otherwise = M.size graph
    update (i, j, k) = do
        ij <- (M.! (i, j))
        ik <- (M.! (i, k))
        kj <- (M.! (k, j))
        M.insert (i, j) (min ij (ik + kj))

data Problem = Problem
    { graph :: M.Map (Text, Text) Int
    , rates :: M.Map Text Int}

search :: Text -> Int -> Problem -> M.Map (S.Set Text) Int
search start total p@Problem{..} = execState (search' start total 0 S.empty) M.empty
    where
    search' x t f v | t <= 0 = pure ()
                    | otherwise = do
        modify $ M.alter (\case
            Nothing -> Just f
            Just f' -> Just $ max f f')  v
        let toVisit = M.filter (> 0) rates
                |> M.filterWithKey (\k _ -> k `S.notMember` v)
                |> M.keys
            visit y = let
                t' = t - (graph M.! (x, y)) - 1
                f' = f + t' * (rates M.! y)
                in search' y t' f' $ S.insert y v
        mapM_ visit toVisit

parseLine :: Parser (Text, Int, [Text])
parseLine = do
    s <- P.string "Valve " >> P.take 2
    r <- P.string " has flow rate=" >> P.decimal
    P.string "; tunnels lead to valves " <|> P.string "; tunnel leads to valve "
    ns <- P.split (P.take 2) (P.string ", ")
    return (s, r, ns)