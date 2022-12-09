module Days.Day09 where

import Util.Util
import Util.Parsing ( Parser )
import qualified Util.Parsing as P

import Data.Char ( isSpace )
import Data.List ( nub, elemIndices )

import qualified Program.RunDay as R (runDay, Day)

runDay :: R.Day
runDay = R.runDay inputParser partA partB -- replace either with partC to run it

type Input = [V2 Int]
type OutputA = Int
type OutputB = Int

inputParser :: Parser Input
inputParser = concat <$> P.lines parseMove

partA :: Input -> OutputA
partA = length . nub . scanl follow 0 . scanl (+) 0

partB :: Input -> OutputB
partB = length . nub . (!! 9) . iterate (scanl follow 0) . scanl (+) 0

partC :: Input -> OutputB
partC = head . elemIndices 1 . map (length . nub) . iterate (scanl follow 0) . scanl (+) 0

touching t h = case h - t of (V2 x y) -> abs x <= 1 && abs y <= 1

follow :: V2 Int -> V2 Int -> V2 Int
follow t h = if touching t h then t else t + signum (h - t)

parseMove :: Parser [V2 Int]
parseMove = do
    c <- P.takeWhile (not . isSpace)
    n <- P.skipSpace >> P.decimal
    return . replicate n $ case c of
        "R" -> V2 0 1; "L" -> V2 0 (-1); "U" -> V2 1 0; "D" -> V2 (-1) 0