module Days.Day21 where

import Util.Util
import Util.Parsing ( Parser )
import qualified Util.Parsing as P

import Control.Monad.State
import qualified Data.Map as M
import Data.Functor ( (<&>) )

import qualified Program.RunDay as R (runDay, Day)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

type Input = Table
type OutputA = Int
type OutputB = Int

inputParser :: Parser Input
inputParser = M.fromList <$> P.many' parseLine

partA :: Input -> OutputA
partA = evalState (eval "root")

partB :: Input -> OutputB
partB = execState changeRoot .> buildTree "root" .> makeEqual 0

data ExprTree = Leaf Int | Expr Int Char ExprTree | Input deriving Show

makeEqual :: Int -> ExprTree -> Int
makeEqual i (Expr l op r) = case op of
    '=' -> makeEqual l r
    '+' -> makeEqual (i - l) r
    '-' -> makeEqual (l - i) r
    '^' -> makeEqual (i + l) r
    '*' -> makeEqual (div i l) r
    '/' -> makeEqual (div l i) r
    '#' -> makeEqual (i * l) r
makeEqual i Input = i

changeRoot :: State Table ()
changeRoot = do
    ~(E l _ r) <- gets (M.! "root")
    modify $ M.insert "root" (E l '=' r)

buildTree :: String -> Table -> ExprTree
buildTree "humn" = pure Input
buildTree s = do
    e <- (M.! s)
    case e of
        L i -> return $ Leaf i
        E s1 op s2 -> do
            e1 <- buildTree s1
            e2 <- buildTree s2
            case (e1, e2) of
                (Leaf i, Leaf j) -> return . Leaf $ parseOp op i j
                (i, Leaf j) -> return $ Expr j (flipOp op) i
                (Leaf i, j) -> return $ Expr i op j
flipOp '/' = '#'
flipOp '-' = '^'
flipOp a = a

data Expr
    = L Int
    | E String Char String
    deriving Show
type Table = M.Map String Expr

eval :: String -> State Table Int
eval s = do
    e <- gets (M.! s)
    case e of
        L i -> return i
        E s1 op s2 -> do
            e1 <- eval s1
            e2 <- eval s2
            let r = parseOp op e1 e2
            modify $ M.insert s (L r)
            return r

parseLine :: Parser (String, Expr)
parseLine = do
    name <- P.ident <* P.string ":"
    expr <- (L <$> P.token P.decimal) <|> (E <$> P.ident <*> P.token P.anyChar <*> P.ident)
    return (name, expr)

parseOp = \case
    '+' -> (+)
    '-' -> (-)
    '/' -> div
    '*' -> (*)