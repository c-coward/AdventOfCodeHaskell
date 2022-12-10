module Days.Day03 where

import Data.Attoparsec.Text
import Util.Util
import Util.Parsing ( Parser )
import qualified Util.Parsing as P

import Data.Char

import qualified Program.RunDay as R (runDay, Day)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

type Input = [String]
type OutputA = Int
type OutputB = Int

inputParser :: Parser Input
inputParser = P.lines P.getLineS

partA :: Input -> OutputA
partA = sum . map (prioritize . overlap . (chunks =<< ((`div` 2) . length)))

partB :: Input -> OutputB
partB = sum . map (prioritize . overlap) . chunks 3

overlap (x:xs) = head [c | c <- x, all (elem c) xs]

prioritize :: Char -> Int
prioritize c | isUpper c = fromEnum c - 38
             | otherwise = fromEnum c - 96