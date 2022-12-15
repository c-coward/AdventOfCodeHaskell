module Days.Day14 where

import Util.Util
import Util.Parsing ( Parser )
import qualified Util.Parsing as P

import Data.Function ( on )
import Data.Ix ( Ix(range) )
import Data.List
import Data.Ord ( comparing )
import qualified Data.Set as S

import qualified Program.RunDay as R (runDay, Day)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

type Input = [[Pair Int]]
type OutputA = Int
type OutputB = ()

inputParser :: Parser Input
inputParser = P.lines parseLine

-- partA :: Input -> OutputA
-- partA = --length . takeWhile snd . (`mapAccumL` repeat P{x=500,y=0}) .
--     (fall =<< y . maximumBy (comparing y)) .
--     S.fromList . concat . concatMap toRanges
partA ps = let
    s = S.fromList . concat . concatMap toRanges $ ps
    ymax = y $ maximumBy (comparing y) s
    rs = scanl (fall ymax) (s, True) $ repeat P{x=500,y=0}
    in length . tail $ takeWhile snd rs

-- partB :: Input -> OutputB
-- partB = undefined
partB ps = let
    s = S.fromList . concat . concatMap toRanges $ ps
    ymax = (+) 2 $ y $ maximumBy (comparing y) s
    rs = scanl (fall' ymax) (s, True) $ repeat P{x=500,y=0}
    in length . tail $ takeWhile snd rs

fall :: Int -> (S.Set (Pair Int), Bool) -> Pair Int -> (S.Set (Pair Int), Bool)
fall ymax (s, b) p@P{..}
    | y >= ymax = (s, False)
    | p{y=y+1} `S.notMember` s       = fall ymax (s, b) p{y=y+1}
    | p{x=x-1,y=y+1} `S.notMember` s = fall ymax (s, b) p{x=x-1,y=y+1}
    | p{x=x+1,y=y+1} `S.notMember` s = fall ymax (s, b) p{x=x+1,y=y+1}
    | otherwise                      = (S.insert p s, True)

fall' :: Int -> (S.Set (Pair Int), Bool) -> Pair Int -> (S.Set (Pair Int), Bool)
fall' ymax (s, b) p@P{..}
    | P{x=500,y=0} `S.member` s      = (s, False)
    | y + 1 == ymax                  = (S.insert p s, True)
    | p{y=y+1} `S.notMember` s       = fall' ymax (s, b) p{y=y+1}
    | p{x=x-1,y=y+1} `S.notMember` s = fall' ymax (s, b) p{x=x-1,y=y+1}
    | p{x=x+1,y=y+1} `S.notMember` s = fall' ymax (s, b) p{x=x+1,y=y+1}
    | otherwise                      = (S.insert p s, True)

parseLine :: Parser [Pair Int]
parseLine = P.split (P <$> P.decimal <*> (P.anyChar *> P.decimal)) $ P.token (P.string "->")

toRanges :: [Pair Int] -> [[Pair Int]]
toRanges (x:y:zs) = range (min x y, max x y) : toRanges (y:zs)
toRanges _ = []