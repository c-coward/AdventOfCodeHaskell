module Days.Day05 where

import Util.Util
import Util.Parsing ( Parser )
import qualified Util.Parsing as P

import qualified Data.Array.IArray as A
import Data.List ( transpose )
import Data.Char ( isDigit )

import qualified Program.RunDay as R (runDay, Day)

import Debug.Trace

runDay :: R.Day
runDay = R.runDay inputParser partA partB

type Input = (Crates, [Move])
type OutputA = String
type OutputB = String

inputParser :: Parser Input
inputParser = do
    cs <- map (dropWhile (== ' ')) <$> parseCrates
    xs <- P.many' (P.token P.decimal)
    is <- P.lines parseMove
    let c = A.listArray (head xs, last xs) cs
    return (c, is)

partA :: Input -> OutputA
partA = uncurry (foldl (moveWith reverse)) .> A.elems .> map head

partB :: Input -> OutputB
partB = uncurry (foldl (moveWith id)) .> A.elems .> map head

type Crates = A.Array Int String

parseCrates :: Parser [String]
parseCrates = transpose <$> P.lines
    (P.split (P.anyChar *> P.satisfy (not . isDigit) <* P.anyChar)
    (P.char ' '))

data Move = Move Int Int Int deriving Show

parseMove :: Parser Move
parseMove = Move <$> (P.token (P.string "move") >> P.decimal)
    <*> (P.token (P.string "from") >> P.decimal)
    <*> (P.token (P.string "to") >> P.decimal)

moveWith :: (String -> String) -> Crates -> Move -> Crates
moveWith f xs (Move n s t) =
    let (m, s') = splitAt n (xs A.! s)
        t' = f m ++ (xs A.! t)
    in xs A.// [(s, s'), (t, t')]