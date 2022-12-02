module Days.Day02 where

import Data.Attoparsec.Text
import Util.Util
import Util.Parsing ( Parser )
import qualified Util.Parsing as P

import Data.Bifunctor ( second )

import qualified Program.RunDay as R (runDay, Day)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

type Input = [(Move, Resp)]
type OutputA = Int
type OutputB = Int

inputParser :: Parser Input
inputParser = P.lines $ (,) <$> parseMove <*> (P.skipSpace >> parseResp)

partA :: Input -> OutputA
partA = map (second respToMove .> score) .> sum

partB :: Input -> OutputB
partB = map (respond .> score) .> sum

data Move = Rock | Paper | Scissors deriving (Show, Eq)
instance Enum Move where
    fromEnum = \case {Rock -> 1; Paper -> 2; Scissors -> 3}
    toEnum = subtract 1 .> (`mod` 3) .> (+ 1) .> \case {1 -> Rock; 2 -> Paper; 3 -> Scissors}
data Resp = X | Y | Z deriving (Show, Eq, Enum)

parseMove = P.choice
    [ P.char 'A' >> pure Rock
    , P.char 'B' >> pure Paper
    , P.char 'C' >> pure Scissors]
parseResp = P.choice
    [ P.char 'X' >> pure X
    , P.char 'Y' >> pure Y
    , P.char 'Z' >> pure Z]

respToMove :: Resp -> Move
respToMove = toEnum . (+ 1) . fromEnum

respond :: (Move, Resp) -> (Move, Move)
respond (m, X) = (m, pred m)
respond (m, Y) = (m, m)
respond (m, Z) = (m, succ m)

score :: (Move, Move) -> Int
score m = fromEnum (snd m) + case m of
    (a, b) | a == b -> 3
    (a, b) | a == succ b -> 0
    (a, b) | a == pred b -> 6
