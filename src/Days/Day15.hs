module Days.Day15 where

import Util.Util
import Util.Parsing ( Parser )
import qualified Util.Parsing as P

import qualified Data.Set as S
import Data.List ( sort, nub )
import Data.Ix

import qualified Program.RunDay as R (runDay, Day)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

type Input = [(Pair Int, Pair Int)]
type OutputA = Int
type OutputB = Int

inputParser :: Parser Input
inputParser = P.lines parseLine

partA :: Input -> OutputA
partA = concatMap (makeInterval 2000000) .> sort .> foldl joinInterval [] .> map size .> sum
partB :: Input -> OutputB
partB = findSolutions (0, 4000000) .> head .> (\P{..} -> x * 4000000 + y)

findSolutions :: (Int, Int) -> Input -> [Pair Int]
findSolutions bounds is =
    let boxes = map box is
        (xs, ys) = unzip boxes
    in [unskew $ P x y |
        x <- candidates xs, y <- candidates ys,
        not . any (inBox (P x y)) $ boxes,
        inBox (unskew $ P x y) (bounds, bounds)]

skew P{..} = P (y + x) (y - x)
unskew P{..} = P (div (x - y) 2) (div (x + y) 2)

box :: (Pair Int, Pair Int) -> ((Int, Int), (Int, Int))
box (s, b) = let
    d = dist s b
    dirs = map (skew . (s +)) [P 0 d, P (-d) 0, P 0 (-d), P d 0]
    xs = (minimum . map x $ dirs, maximum . map x $ dirs)
    ys = (minimum . map y $ dirs, maximum . map y $ dirs)
    in (xs, ys)
candidates bs = nub . concat $ [[l - 1, h + 1] | (l, h) <- bs]

inBox :: Pair Int -> ((Int, Int), (Int, Int)) -> Bool
inBox P{..} (xs, ys) = inRange xs x && inRange ys y

dist :: Pair Int -> Pair Int -> Int
dist a b = case a - b of P{..} -> abs x + abs y

type Interval = (Int, Int, S.Set Int)

makeInterval :: Int -> (Pair Int, Pair Int) -> [Interval]
makeInterval m (s, b) =
    let d = dist s b
        d' = dist s (P {x=x s,y=m})
        l = x s - d + d'
        r = x s + d - d'
        i = S.fromList [x b | y b == m]
    in [(l, r, i) | d' <= d]

joinInterval :: [Interval] -> Interval -> [Interval]
joinInterval [] x = [x]
joinInterval (c@(a,b,i):cs) z@(x,y,j) = if x <= b then (a,max b y, i `S.union` j):cs
    else z:c:cs

size :: Interval -> Int
size (a, b, i) = b - a + 1 - S.size i

parseLine :: Parser (Pair Int, Pair Int)
parseLine = do
    xs <- P.string "Sensor at x=" >> P.signed P.decimal
    ys <- P.string ", y=" >> P.signed P.decimal
    xb <- P.string ": closest beacon is at x=" >> P.signed P.decimal
    yb <- P.string ", y=" >> P.signed P.decimal
    return (P xs ys, P xb yb)


{-
 0.5  0.5 x = y + x
-0.5  0.5 y = y - x
-}