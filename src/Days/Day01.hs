module Days.Day01 where

import qualified Data.Attoparsec.Text as T
import Util.Util
import Util.Parsing

import qualified Program.RunDay as R (runDay, Day)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

type Input = ()
type OutputA = ()
type OutputB = ()

inputParser :: T.Parser Input
inputParser = undefined

partA :: Input -> OutputA
partA = undefined

partB :: Input -> OutputB
partB = undefined