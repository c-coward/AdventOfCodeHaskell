module Days.Day02 where

import Data.Attoparsec.Text
import Util.Util
import Util.Parsing ( Parser )
import qualified Util.Parsing as P

import Data.Functor ( ($>) )
import Data.Bifunctor ( second )

import qualified Program.RunDay as R (runDay, Day)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

type Input = [(Move, Resp)]
type OutputA = Int
type OutputB = Int

parseMove = P.char 'A' $> Rock <|> P.char 'B' $> Paper <|> P.char 'C' $> Scissors
parseResp = P.char 'X' $> X <|> P.char 'Y' $> Y <|> P.char 'Z' $> Z

inputParser :: Parser Input
inputParser = P.lines $ (,) <$> parseMove <*> (P.skipSpace >> parseResp)

partA :: Input -> OutputA
partA = foldr (second respToMove .> score .> (+)) 0

partB :: Input -> OutputB
partB = foldr (respond .> score .> (+)) 0

data Move = Rock | Paper | Scissors deriving (Show, Eq)
instance Enum Move where -- Needs explicit implementation to encode cyclic behavior
    fromEnum = \case {Rock -> 1; Paper -> 2; Scissors -> 3}
    toEnum = subtract 1 .> (`mod` 3) .> (+ 1) .> \case {1 -> Rock; 2 -> Paper; 3 -> Scissors}
data Resp = X | Y | Z deriving (Show, Eq, Enum)

respToMove :: Resp -> Move
respToMove = toEnum . (+ 1) . fromEnum

respond :: (Move, Resp) -> (Move, Move)
respond (m, r) = (m,) $ case r of {X -> pred m; Y -> m; Z -> succ m}

score :: (Move, Move) -> Int
score (a, b) = fromEnum b + if a == b then 3
    else if a == succ b then 0 else 6
