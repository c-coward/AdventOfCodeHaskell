module Days.Day17 where

import Util.Util
import Util.Parsing ( Parser )
import qualified Util.Parsing as P

import Control.Monad.State
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List ( maximumBy, minimumBy, intersperse, groupBy, sort )
import Data.Ord ( comparing )
import Data.Ix ( inRange )

import Debug.Trace

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
partB = makeSim .> execState (simulate' 1000000000000) .> game .> height

rockList :: [[Pair Int]]
rockList =
    [ [P 2 0, P 3 0, P 4 0, P 5 0]
    , [P 3 0, P 3 1, P 3 2, P 2 1, P 4 1]
    , [P 2 0, P 3 0, P 4 0, P 4 1, P 4 2]
    , [P 2 0, P 2 1, P 2 2, P 2 3]
    , [P 2 0, P 3 0, P 2 1, P 3 1]]

type PointSet = S.Set (Pair Int)
data Sim = Sim
    { rocks :: [[Pair Int]]
    , jets :: String
    , game :: PointSet
    , jetSize :: Int
    , jetMoves :: Int
    , moves :: Int}

drawGame :: PointSet -> String
drawGame g = unlines [[if P x y `S.member` g then '#' else ' ' | x <- [0..6]] | let (P _ yl, P _ ym) = box g, y <- reverse [yl..ym]]

makeSim :: [Char] -> Sim
makeSim s = Sim (cycle rockList)
    (cycle s |> intersperse 'v')
    (S.fromList [P x 0 | x <- [0..6]])
    (length s)
    0
    0

height :: PointSet -> Int
height = y . maximumBy (comparing y)

box :: PointSet -> (Pair Int, Pair Int)
box = do
    xl <- x . minimumBy (comparing x)
    yl <- y . minimumBy (comparing y)
    xm <- x . maximumBy (comparing x)
    ym <- y . maximumBy (comparing y)
    return (P xl yl, P xm ym)

data Flood = Flood
    { dat :: PointSet
    , vis :: PointSet
    , ret :: PointSet }

runFlood :: PointSet -> PointSet
runFlood g = ret . execState (mapM_ flood [P x y | let y = height g, x <- [0..6]])
    $ Flood g S.empty S.empty

flood :: Pair Int -> State Flood ()
flood c@P{..} = do
    Flood{..} <- get
    unless (S.member c vis || not (box dat `inRange` c)) $ do
        modify $ \f -> f{vis=S.insert c vis}
        if S.member c dat then modify $ \f -> f{ret=S.insert c ret}
            else mapM_ flood [P (x + 1) y, P x (y + 1), P (x - 1) y, P x (y - 1)]

simulate :: Int -> State Sim ()
simulate k = mapM_ (const oneStep) [1..k]

oneStep, reduceGame, oneRock, initRock, takeSteps :: State Sim ()
oneStep = oneRock >> reduceGame
reduceGame = modify $ \s@Sim{..} -> s{game=runFlood game}
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

toMoves :: Char -> PointSet -> [Pair Int] -> [Pair Int]
toMoves '<' = left
toMoves '>' = right

onLeft, onRight, atRest :: PointSet -> [Pair Int] -> Bool
onLeft g r = any (x .> (== 0)) r || any (subtract (P 1 0) .> (`S.member` g)) r
onRight g r = any (x .> (== 6)) r || any ((+ P 1 0) .> (`S.member` g)) r
atRest g = map (subtract $ P 0 1) .> any (`S.member` g)

left, right, down :: PointSet -> [Pair Int] -> [Pair Int]
left g r = if onLeft g r then r else map (subtract $ P 1 0) r
right g r = if onRight g r then r else map (+ P 1 0) r
down g r = if atRest g r then r else map (subtract $ P 0 1) r

data CycleInfo = CI
    { gNorm :: PointSet
    , rMod :: Int
    , jMod :: Int } deriving (Eq, Ord)

normalize :: PointSet -> PointSet
normalize = do
    yl <- y . minimumBy (comparing y)
    S.map (subtract (P 0 yl))

simToCI :: Sim -> CycleInfo
simToCI Sim{..} = CI (normalize game) (moves `mod` 5) (jetMoves `mod` jetSize)

data CycleBegin = CB
    { gBase :: PointSet
    , hDiff :: Int
    , moves' :: Int
    , mDiff :: Int
    , cRock :: [[Pair Int]]
    , cJets :: String}

cbToSim :: CycleBegin -> Sim -- Jet size/moves don't matter when reconstructing
cbToSim CB{..} = Sim cRock cJets gBase 1 0 moves'

addCycles :: Int -> CycleBegin -> CycleBegin
addCycles k CB{..} = CB
    (S.map (+ P 0 (k * hDiff)) gBase)
    hDiff
    (moves' + k * mDiff)
    mDiff
    cRock
    cJets

simulate' :: Int -> State Sim ()
simulate' k = do
    cb <- detectCycle M.empty
    if k < moves' cb then simulate k else do
        let k' = k - moves' cb
            cs = div k' $ mDiff cb
            cb' = addCycles cs cb
            k'' = k - moves' cb'
        modify $ \_ -> cbToSim cb'
        simulate k''

detectCycle :: M.Map CycleInfo Sim -> State Sim CycleBegin
detectCycle m = do
    s <- get
    let ci@CI{..} = simToCI s
    if M.member ci m
        then do
            let s' = m M.! ci
                heightS = height (game s)
                heightS' = height (game s')
            return $ CB (game s')
                (heightS - heightS')
                (moves s')
                (moves s - moves s')
                (rocks s')
                (jets s')
        else oneStep >> detectCycle (M.insert ci s m)