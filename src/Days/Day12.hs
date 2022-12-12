module Days.Day12 where

import Util.Util
import Util.Parsing ( Parser )
import qualified Util.Parsing as P

import qualified Data.Array.IArray as A
import Control.Monad.State
import Data.List (  nub )

import Debug.Trace

import qualified Program.RunDay as R (runDay, Day)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

type Input = Grid
type OutputA = Int
type OutputB = Int

inputParser :: Parser Input
inputParser = inpToGrid <$> P.lines P.getLineS

partA :: Input -> OutputA
partA = evalState (starts >>= bfs)

partB :: Input -> OutputB
partB = evalState (startsA >>= bfs)
    -- traceId . unlines . chunks 8 . evalState (startsA >>=
    -- steps >>= steps >>= steps >>= steps
    -- >> gets A.elems)

type Grid = A.Array (Pair Int) Char
type BFS = State Grid

bfs :: [Pair Int] -> BFS Int
bfs xs = do
    g <- get
    if any ((g A.!) .> (== 'E')) xs
        then return 0
        else (+ 1) <$> (steps xs >>= bfs)

steps :: [Pair Int] -> BFS [Pair Int]
steps xs = do
    xs' <- gets $ filter (not . (`elem` xs)) . nub . (`concatMap` xs) . neighbors
    modify (A.// map (, '|') xs) >> return xs'

starts :: BFS [Pair Int]
starts = gets $ A.assocs .> filter (snd .> (== 'S')) .> map fst

startsA :: BFS [Pair Int]
startsA = gets $ A.assocs .> filter (snd .> (`elem` ['a','S'])) .> map fst

inpToGrid :: [String] -> Grid
inpToGrid xs = A.listArray (P 1 1, P (length . head $ xs) (length xs)) $ concat xs

dirs = [P 0 1, P 1 0, P 0 (-1), P (-1) 0]
neighbors :: Grid -> Pair Int -> [Pair Int]
neighbors g p = map (p +) dirs |> filter (A.inRange $ A.bounds g)
    |> filter (canStep (g A.! p) . (g A.!))

canStep :: Char -> Char -> Bool
canStep 'S' b = canStep 'a' b
canStep a 'E' = canStep a 'z'
canStep a b = fromEnum b - fromEnum a <= 1