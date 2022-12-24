module Days.Day24 where

import Util.Util
import Util.Parsing ( Parser )
import qualified Util.Parsing as P

import Data.List ( nub )

import Control.Monad.State

import qualified Program.RunDay as R (runDay, Day)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

type Input = Sim
type OutputA = Int
type OutputB = Int

inputParser :: Parser Input
inputParser = P.asString $ lines .> parseSim

partA :: Input -> OutputA
partA = iterate runSim .> takeWhile (not . atEnd) .> length

partB :: Input -> OutputB
partB = iterate runSim .> takeWhile (not . endStartEnd) .> length

type Bounds = (Pair Int, Pair Int)
type Point = Pair Int
data Sim = Sim
    { pos :: [(Point, Int)]
    , bliz :: [(Point, Char)]
    , bounds :: Bounds} deriving Show

atEnd, endStartEnd :: Sim -> Bool
atEnd Sim{..} = any (snd .> (== 1)) pos
endStartEnd Sim{..} = any (snd .> (== 3)) pos

runSim = execState oneMinute

oneMinute :: State Sim ()
oneMinute = moveBlizzard >> movePos

dirs = [P 0 0, P 0 1, P 1 0, P 0 (-1), P (-1) 0]
canMove :: (Point, Int) -> State Sim Bool
canMove p = do
    cond1 <- gets $ bliz .> map fst .> elem (fst p)
    cond2 <- gets $ bounds .> onBounds (fst p)
    return . not $ cond1 || cond2

moveBlizzard :: State Sim ()
moveBlizzard = do
    b <- gets bounds
    bliz' <- gets $ bliz .> map (moveBliz .> wrap b)
    modify $ \sim -> sim{bliz=bliz'}

movePos :: State Sim ()
movePos = do
    pos' <- gets $ pos .> concatMap (\(p, h) -> map ((+ p) .> (, h)) dirs)
    validPos <- filterM canMove pos'
    nextPos <- mapM updateHistory validPos
    modify $ \sim -> sim{pos=nub nextPos}

updateHistory :: (Point, Int) -> State Sim (Point, Int)
updateHistory (p, h) = do
    b <- gets bounds
    let h'  | h == 0 && p == end b = 1
            | h == 1 && p == start b = 2
            | h == 2 && p == end b = 3
            | otherwise = h
    return (p, h')

moveBliz :: (Point, Char) -> (Point, Char)
moveBliz (p, c) = (, c) $ case c of
    '>' -> p + P 1 0
    '<' -> p - P 1 0
    'v' -> p + P 0 1
    '^' -> p - P 0 1

start, end :: Bounds -> Point
start (P{..}, _) = P (x + 1) y
end (_, P{..}) = P (x - 1) y

wrap :: Bounds -> (Point, Char) -> (Point, Char)
wrap (P xl yl, P xh yh) (P{..}, c)
    | x == xl = (P (xh - 1) y, c)
    | x == xh = (P (xl + 1) y, c)
    | y == yl = (P x (yh - 1), c)
    | y == yh = (P x (yl + 1), c)
    | otherwise = (P x y, c)

onBounds :: Point -> Bounds -> Bool
onBounds p@P{..} b@(P xl yl, P xh yh) =
    let c1 = x <= xl || x >= xh
        c2 = y <= yl || y >= yh
        c3 = notElem p [start b, end b]
    in (c1 || c2) && c3

parseSim :: [String] -> Sim
parseSim = do
    yh <- length
    xh <- length . head
    bliz <- parseBliz
    let bounds = (P 1 1, P xh yh)
    return $ Sim [(start bounds, 0)] bliz bounds

parseBliz :: [String] -> [(Point, Char)]
parseBliz = zip [1..] .> concatMap (uncurry parseLine)

parseLine :: Int -> String -> [(Point, Char)]
parseLine y = zip [P x y | x <- [1..]] .> filter (snd .> (`elem` ("<>v^" :: String)))