module Days.Day03 where

import Data.Attoparsec.Text
import Util.Util
import Util.Parsing ( Parser )
import qualified Util.Parsing as P

import Data.Char
import Data.List

import qualified Program.RunDay as R (runDay, Day)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

type Input = [(String, String)]
type OutputA = Int
type OutputB = ()

inputParser :: Parser Input
inputParser = P.asString $ lines .> map (\s -> splitAt (length s `div` 2) s)

partA :: Input -> OutputA
partA = sum . map (sum . map prioritize . overlap)

partB :: Input -> OutputB
partB = undefined

overlap (s1, s2) = nub [c | c <- s1, c `elem` s2]

prioritize :: Char -> Int
prioritize c | isUpper c = fromEnum c - 38
             | otherwise = fromEnum c - 96