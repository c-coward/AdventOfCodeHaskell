module Days.Day09 where

import Util.Util
import Util.Parsing ( Parser )
import qualified Util.Parsing as P

import Data.Char ( isSpace )
import Data.List ( nub )

import qualified Program.RunDay as R (runDay, Day)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

type Input = [Pair Int]
type OutputA = Int
type OutputB = Int

inputParser :: Parser Input
inputParser = concat <$> P.lines parseMove

partA :: Input -> OutputA
partA = length . nub . scanl follow (P 0 0) . scanl (+) (P 0 0)

partB :: Input -> OutputB
partB = length . nub . (!! 9) . iterate (scanl follow (P 0 0)) . scanl (+) (P 0 0)

-- Basic moves
r = P 0 1
l = P 0 (-1)
u = P 1 0
d = P (-1) 0

follow :: Pair Int -> Pair Int -> Pair Int
follow t h = case h - t of
    p@P{..} | (x == 0 && abs y > 1) || (y == 0 && abs x > 1) -> t + signum p
            | x /= 0 && y /= 0 && (abs x > 1 || abs y > 1) -> t + signum p
            | otherwise -> t

parseMove :: Parser [Pair Int]
parseMove = do
    c <- P.takeWhile (not . isSpace)
    n <- P.skipSpace >> P.decimal
    return . replicate n $ case c of {"R" -> r; "L" -> l; "U" -> u; "D" -> d}