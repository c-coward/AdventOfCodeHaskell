module Days.Day23 where

import Util.Util
import Util.Parsing ( Parser )
import qualified Util.Parsing as P

import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State
import Data.List ( maximumBy, minimumBy )
import Data.Ord ( comparing )
import Control.Arrow ( (&&&) )

import Debug.Trace

import qualified Program.RunDay as R (runDay, Day)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

type Input = Sim
type OutputA = Int
type OutputB = Int

inputParser :: Parser Input
inputParser = P.asString $ makeSim . parseLines . lines

lineY :: Int -> String -> [Pair Int]
lineY y = zip [P x y | x <- [0..]] .> filter (snd .> (== '#')) .> map fst
parseLines :: [String] -> S.Set (Pair Int)
parseLines = zip [0..] .> concatMap (uncurry lineY) .> S.fromList

-- partA :: Input -> OutputA
partA = iterate runNextSteps .> (!! 10) .> locs
    .> rectangle &&& length .> uncurry (-)

partB :: Input -> OutputB
partB = iterate runNextSteps .> map locs .> zip <*> tail
    .> takeWhile (uncurry (/=)) .> length .> (+ 1)

rectangle :: S.Set (Pair Int) -> Int
rectangle = do
    yh <- y . maximumBy (comparing y)
    yl <- y . minimumBy (comparing y)
    xh <- x . maximumBy (comparing x)
    xl <- x . minimumBy (comparing x)
    return $ (yh - yl + 1) * (xh - xl + 1)

data Sim = Sim
    { locs :: S.Set (Pair Int)
    , opts :: [([Pair Int], Pair Int)]} deriving Show

makeSim :: S.Set (Pair Int) -> Sim
makeSim s = Sim s choices

type PropMap = M.Map (Pair Int) [Pair Int]

runNextSteps = execState actNextSteps

actNextSteps :: State Sim ()
actNextSteps = do
    locs' <- actProp <$> getNextSteps
    modify $ \s -> s{locs=locs', opts=tail $ opts s}

getNextSteps :: State Sim PropMap
getNextSteps = do
    ps <- gets $ locs .> S.elems
    ps' <- gets $ \s -> map (makeChoice s) ps
    return $ foldr addProp M.empty (zip ps ps')

actProp :: PropMap -> S.Set (Pair Int)
actProp = S.fromList . concatMap act . M.assocs where
    act (p', [p]) = [p']
    act (_, ps)   = ps

addProp :: (Pair Int, Pair Int) -> PropMap -> PropMap
addProp (p, p') = M.findWithDefault [] p' >>= \case
    [] -> M.insert p' [p]
    ps -> M.adjust (p:) p'

allDirs = [P x y | x <- [-1..1], y <- [-1..1], (x,y) /= (0,0)]
choices :: [([Pair Int], Pair Int)]
choices = cycle
    [ ([P x (-1) | x <- [-1..1]], P 0 (-1))
    , ([P x 1 | x <- [-1..1]], P 0 1)
    , ([P (-1) y | y <- [-1..1]], P (-1) 0)
    , ([P 1 y | y <- [-1..1]], P 1 0)]

makeChoice :: Sim -> Pair Int -> Pair Int
makeChoice s p = case map (runChoice s p) ((allDirs, 0) : opts s)
    |> take 5 |> filter (/= Nothing) of
        [] -> p
        (Just p':_) -> p'

runChoice :: Sim -> Pair Int -> ([Pair Int], Pair Int) -> Maybe (Pair Int)
runChoice Sim{..} p (ps, p') | not $ any ((`S.member` locs) . (p +)) ps = Just $ p + p'
runChoice _ _ p = Nothing