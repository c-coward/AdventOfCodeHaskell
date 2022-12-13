{-# LANGUAGE StandaloneDeriving #-}

module Days.Day13 where

import Util.Util
import Util.Parsing ( Parser )
import qualified Util.Parsing as P

import Data.List ( sort )
import Debug.Trace

import qualified Program.RunDay as R (runDay, Day)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

type Input = [(RecList Int, RecList Int)]
type OutputA = Int
type OutputB = Int

inputParser :: Parser Input
inputParser = P.split ((,) <$> P.line parseRecList <*> P.line parseRecList) P.endOfLine
deriving instance Show a => Show (RecList a)
deriving instance Show a => Show (Item a)

partA :: Input -> OutputA
partA = sum . map fst . filter snd . zip [1..] . map (uncurry (<=))

partB :: Input -> OutputB
partB = product . map fst . filter (snd .> (`elem` dividers)) . zip [1..] . sort . (dividers ++) . concatMap (\(a, b) -> [a,b])

dividers :: [RecList Int]
dividers = [L (N 2 :> Nil) :> Nil, L (N 6 :> Nil) :> Nil]

data RecList a where
    Nil :: RecList a
    (:>) :: Item a -> RecList a -> RecList a
infixr :>
deriving instance Eq a => Eq (RecList a)
instance Ord a => Ord (RecList a) where
    compare Nil Nil = EQ
    compare Nil _ = LT
    compare _ Nil = GT
    compare (a :> b) (c :> d) = case compare a c of
        EQ -> compare b d
        o -> o

data Item a where
    N :: a -> Item a
    L :: RecList a -> Item a
deriving instance Eq a => Eq (Item a)
instance Ord a => Ord (Item a) where
    compare (N a) (N b) = compare a b
    compare (L a) (L b) = compare a b
    compare (N a) b = compare (L $ N a :> Nil) b
    compare a (N b) = compare a (L $ N b :> Nil)

parseRecList :: Parser (RecList Int)
parseRecList = P.string "[" >> foldr (:>) Nil <$> P.split parseItem (P.string ",") <* P.string "]"

parseItem :: Parser (Item Int)
parseItem = (N <$> P.decimal) <|> (L <$> parseRecList)