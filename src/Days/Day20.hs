module Days.Day20 where

import Util.Util
import Util.Parsing ( Parser )
import qualified Util.Parsing as P

import Control.Monad.State
import Data.CircularList

import qualified Program.RunDay as R (runDay, Day)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

type Input = [Int]
type OutputA = Int
type OutputB = ()

inputParser :: Parser Input
inputParser = P.many' $ P.token $ P.signed P.decimal

-- partA :: Input -> OutputA
partA = makeMixer 
    -- .> evalState (mixAll >> groveSum)
    .> execState mixAll

partB :: Input -> OutputB
partB = undefined

fromJust = \case Just a -> a

data Elem = Elem {val :: Int, idx :: Int, shift :: Int}
instance Show Elem where show Elem{..} = show val
type Mixer = CList Elem

makeMixer :: [Int] -> Mixer
makeMixer = length >>= \l -> fromList . zipWith (\i x -> Elem x i (mod x $ l - 1)) [1..]

focus' = fromJust . focus
find p = fromJust . findRotateTo p

mix :: Int -> State Mixer ()
mix i = do
    m' <- gets $ find (idx .> (== i))
    let e = focus' m'
    modify $ removeL .> rotN (shift e) .> insertL e

mixAll :: State Mixer ()
mixAll = gets (length .> \l -> [1..l]) >>= mapM_ mix

groveSum :: State Mixer [Int]
groveSum = do
    m' <- gets $ find (val .> (== 0))
    let f1 = m' |> rotN 1000 |> focus' |> val
        f2 = m' |> rotN 2000 |> focus' |> val
        f3 = m' |> rotN 3000 |> focus' |> val
    return [f1,f2,f3]

-- data Mix = Mix
--     { order :: [Int]
--     , file :: CList Int
--     , fsize :: Int}
-- makeE xs = Mix xs (fromList xs) (length xs)

-- mix :: Int -> State Mix ()
-- mix x = do
--     s <- gets $ fsize .> subtract 1
--     f' <- gets $ file .> find x .> removeL .> rotN (mod x s) .> insertL x 
--     modify $ \f -> f{file=f'}

-- mixAll = gets order >>= mapM_ mix

-- getGroves :: [Int] -> State Mix Int
-- getGroves xs = fmap sum $ modify (\f@Mix{..} -> f{file=find 0 file}) >> mapM getVal xs

-- getVal :: Int -> State Mix Int
-- getVal i = do
--     s <- gets fsize
--     gets $ file .> rotN (mod i s) .> focus'