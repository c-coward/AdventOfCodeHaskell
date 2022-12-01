module Days.Day03 where

import Data.Attoparsec.Text
import Util.Util
import Util.Parsing ( Parser )
import qualified Util.Parsing as P

import qualified Program.RunDay as R (runDay, Day)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

type Input = ()
type OutputA = ()
type OutputB = ()

inputParser :: Parser Input
inputParser = undefined

partA :: Input -> OutputA
partA = undefined

partB :: Input -> OutputB
partB = undefined