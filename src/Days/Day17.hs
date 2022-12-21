module Days.Day17 where

import Util.Util
import Util.Parsing ( Parser )
import qualified Util.Parsing as P

import Control.Monad.State
import qualified Data.Set as S
import Data.List ( maximumBy, intersperse )
import Data.Ord ( comparing )

import qualified Program.RunDay as R (runDay, Day)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

type Input = String
type OutputA = Int
type OutputB = Int

inputParser :: Parser Input
inputParser = P.getLineS

partA :: Input -> OutputA
partA = makeSim .> execState (simulate 2022) .> game .> height

partB :: Input -> OutputB
partB = makeSim .> evalState detectCycle

detectCycle :: State Sim Int
detectCycle = do
    oneRock
    Sim{..} <- get
    if isFlat game && mod moves 5 == 0 && mod jetMoves jetSize == 0 && moves > 0
        then return moves
        else detectCycle
    

isFlat g = all (`S.member` g) [P x h | let h = height g, x <- [0 .. 6]]

rockList =
    [ [P 2 0, P 3 0, P 4 0, P 5 0]
    , [P 3 0, P 3 1, P 3 2, P 2 1, P 4 1]
    , [P 2 0, P 3 0, P 4 0, P 4 1, P 4 2]
    , [P 2 0, P 2 1, P 2 2, P 2 3]
    , [P 2 0, P 3 0, P 2 1, P 3 1]]

onLeft g p = any (x .> (== 0)) p || any (subtract (P 1 0) .> (`S.member` g)) p
onRight g p = any (x .> (== 6)) p || any ((+ P 1 0) .> (`S.member` g)) p
left g p = if onLeft g p then p else map (subtract $ P 1 0) p
right g p = if onRight g p then p else map (+ P 1 0) p

height :: S.Set (Pair Int) -> Int
height = y . maximumBy (comparing y)

atRest g = map (subtract $ P 0 1) .> any (`S.member` g)
data Sim = Sim
    { rocks :: [[Pair Int]]
    , jets :: String
    , game :: S.Set (Pair Int)
    , jetSize :: Int
    , jetMoves :: Int
    , moves :: Int}
makeSim s = Sim (cycle rockList)
    (cycle s |> intersperse 'v')
    (S.fromList [P x 0 | x <- [0..6]])
    (length s)
    0
    0
toMoves '<' = left
toMoves '>' = right

simulate :: Int -> State Sim ()
simulate k = do
    m <- gets moves
    when (m < k) $ oneRock >> simulate k

oneRock = do
    initRock
    takeSteps
    modify $ \s@Sim{rocks=r:rs} -> s{rocks=rs , moves=moves s + 1
        , game=game s |> flip (foldr S.insert) r}

initRock = do
    h <- gets $ game .> height
    modify $ \s@Sim{rocks=r:rs} -> s{rocks=map (+ P 0 (4 + h)) r:rs}

takeSteps = do
    g <- gets game
    r <- gets $ rocks .> head
    j <- gets $ jets .> head
    let m = case j of
            'v' -> down g
            _   -> toMoves j g
    modify $ \s@Sim{rocks=(r:rs), jets=(_:js)} ->
        s{rocks=m r:rs, jets=js, jetMoves=jetMoves s + 1}
    unless (atRest g r && j == 'v') takeSteps
    
down g r = if atRest g r then r else map (subtract $ P 0 1) r