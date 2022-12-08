module Days.Day08 where

import Util.Util
import Util.Parsing ( Parser )
import qualified Util.Parsing as P

import Data.Char (digitToInt)
import Data.List

import qualified Program.RunDay as R (runDay, Day)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

type Input = [[Int]]
type OutputA = Int
type OutputB = Int

inputParser :: Parser Input
inputParser = P.lines $ map digitToInt <$> P.getLineS

partA :: Input -> OutputA
partA = transform .> scanGrid visible (-1) .> untransform .> foldGrid (||) .> concat .> filter id .> length

partB :: Input -> OutputB
partB = transform .> scanGrid scenic [] .> untransform .> foldGrid (*) .> concat .> maximum

-- List transformations to get rows and columns, forwards and backwards
transforms = [id, transpose, map reverse, map reverse . transpose]
untransforms = [id, transpose, map reverse, transpose . map reverse]

transform x = map ($ x) transforms
untransform = zipWith ($) untransforms

visible :: Int -> Int -> (Int, Bool)
visible b a = (max a b, a > b) 

scenic :: [Int] -> Int -> ([Int], Int)
scenic b a = (a : b, length $ takeUntil a b)

takeUntil :: Ord a => a -> [a] -> [a]
takeUntil p as@(~(x:xs))
    | null as = []
    | x < p = x : takeUntil p xs
    | otherwise = [x]

scanGrid :: (s -> a -> (s, b)) -> s -> [[[a]]] -> [[[b]]]
scanGrid f z = map (map (snd . mapAccumL f z))

foldGrid :: (a -> a -> a) -> [[[a]]] -> [[a]]
foldGrid f = foldl1 (zipWith (zipWith f))