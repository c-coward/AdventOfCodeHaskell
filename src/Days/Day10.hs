module Days.Day10 where

import Data.Attoparsec.Text
import Util.Util
import Util.Parsing ( Parser )
import qualified Util.Parsing as P

import Data.Functor
import Debug.Trace -- for cheating!

import qualified Program.RunDay as R (runDay, Day)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

type Input = [Pair Int]
type OutputA = Int
type OutputB = String

inputParser :: Parser Input
inputParser = concat <$> P.lines parseCmd

partA :: Input -> OutputA
partA = sum . map (\P{..} -> x * y) . filter (\P{..} -> mod x 40 == 20 && x <= 220) . scanl (+) 1

partB :: Input -> OutputB
partB = traceId . unlines . chunks 40 . map (\P{..} -> if abs (mod (x - 1) 40 - y) <= 1 then '#' else ' ') . scanl (+) 1

parseCmd :: Parser [Pair Int]
parseCmd = do
    P.string "addx" >> P.space
    a <- P 1 <$> P.signed P.decimal
    return [P 1 0, a]
    <|> do
    P.string "noop" $> [P 1 0]