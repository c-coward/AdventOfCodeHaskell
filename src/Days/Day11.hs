module Days.Day11 where

import Util.Util
import Util.Parsing ( Parser )
import qualified Util.Parsing as P

import Control.Monad.State
import qualified Data.Array.IArray as A
import Data.Functor ( ($>) )
import Data.List ( sort )
import Data.Maybe ( fromMaybe )

import qualified Program.RunDay as R (runDay, Day)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

type Input = A.Array Int Monkey
type OutputA = Int
type OutputB = Int

inputParser :: Parser Input
inputParser = (A.listArray =<< ((0,) . subtract 1 . length)) <$>
    P.split parseMonkey P.endOfLine

partA :: Input -> OutputA
partA = product . take 2 . reverse . sort . map inspections . A.elems .
    execState (forM_ [1..20] $ const doRound) .
    A.amap (\m@M{..} -> m{alter=(`div` 3) . alter})

partB :: Input -> OutputB
partB = product . take 2 . reverse . sort . map inspections . A.elems .
    execState (forM_ [1..10000] $ const doRound) .
    -- (\as -> A.amap (\m@M{..} -> m{alter=(`mod` getMods as) . alter}) as)
    ((\i -> A.amap (\m@M{..} -> m{alter=(`mod` i) . alter})) =<< getMods)

getMods :: A.Array Int Monkey -> Int
getMods = foldl1 lcm . A.amap (fst . throw)

data Monkey = M
    { items :: [Int]
    , alter :: Int -> Int
    , throw :: (Int, (Int, Int))
    , inspections :: Int}
instance Show Monkey where
    show M{..} = show items

runThrow :: Monkey -> Int -> Int
runThrow M{throw=(a,(b,c))} i = if (i `mod` a) == 0 then b else c

type MonkeyState = State (A.Array Int Monkey)

inspect :: Int -> MonkeyState ()
inspect i = do
    m@M{items=is,..} <- gets (A.! i)
    let item = alter . head $ is
    let j = runThrow m item
    m' <- gets (A.! j)
    modify (A.// [(i, m{items=tail is, inspections=inspections + 1}),
                  (j, m'{items=items m' ++ [item]})])

inspectAll :: Int -> MonkeyState ()
inspectAll i = do
    m@M{..} <- gets (A.! i)
    case items of
        [] -> return ()
        _  -> inspect i >> inspectAll i

doRound :: MonkeyState ()
doRound = do
    (i, j) <- gets A.bounds
    forM_ [i..j] inspectAll

parseMonkey :: Parser Monkey
parseMonkey = do
    P.line P.getLineS
    P.token $ P.string "Starting items: "
    is <- P.line $ P.split P.decimal (P.string ", ")
    alter <- parseAlter
    throw <- parseThrow
    return $ M is alter throw 0

parseAlter :: Parser (Int -> Int)
parseAlter = do
    P.token (P.string "Operation: new =")
    op1 <- P.token parseOperand
    op  <- P.token parseOperator
    op2 <- P.token parseOperand
    return $ \x -> fromMaybe x op1 `op` fromMaybe x op2

parseThrow :: Parser (Int, (Int, Int))
parseThrow = do
    n <- P.token (P.string "Test: divisible by") >> P.line P.decimal
    jT <- P.token (P.string "If true: throw to monkey") >> P.line P.decimal
    jF <- P.token (P.string "If false: throw to monkey") >> P.line P.decimal
    return (n, (jT, jF))

parseOperand :: Parser (Maybe Int)
parseOperand = P.string "old" $> Nothing <|> Just <$> P.decimal

parseOperator :: Parser (Int -> Int -> Int)
parseOperator = P.string "+" $> (+) <|> P.string "*" $> (*)