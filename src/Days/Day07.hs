module Days.Day07 where

import Util.Util
import Util.Parsing ( Parser )
import qualified Util.Parsing as P

import Data.Tree
import Data.Tree.Zipper
import Data.Maybe (fromJust, maybe)
import Data.Either (rights)

import qualified Program.RunDay as R (runDay, Day)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

type Input = [Cmd]
type OutputA = Int
type OutputB = Int

inputParser :: Parser Input
inputParser = P.lines parseCmd

partA :: Input -> OutputA
partA =
    sum . filter (<= 100000) . flatten .
    sizeDirs . toTree . root . foldl runCmd rootDir

partB :: Input -> OutputB
partB = solve . flatten .
    sizeDirs . toTree . root . foldl runCmd rootDir

solve :: [Int] -> Int
solve xs@(x:_) = minimum $ filter (>= (30000000 - (70000000 - x))) xs

sizeDirs :: FTree -> Tree Int
sizeDirs = sizeFiles .> toRights

toRights :: Tree (Either Int Int) -> Tree Int
toRights (Node (Left i) _) = pure 0
toRights (Node (Right i) ts) = Node i (map toRights ts)

sizeFiles :: FTree -> Tree (Either Int Int)
sizeFiles (Node File{..} _) = pure . Left $ size
sizeFiles (Node _ sF) = let ts = map sizeFiles sF
    in Node (Right . foldr ((+) . getSize) 0 $ ts) ts

getSize :: Tree (Either Int Int) -> Int
getSize (Node (Left i) ts) = i -- + sum (map getSize ts)
getSize (Node (Right i) ts) = i -- + sum (map getSize ts)


data Cmd = Cd String | Ls [File] deriving Show

parseCmd = (P.string "$ cd " >> Cd <$> P.getLineS)
    <|> (P.line (P.string "$ ls") >> Ls <$> P.split parseFile P.endOfLine)

runCmd :: FileSys -> Cmd -> FileSys
runCmd f = \case
    Cd "/"  -> root f
    Cd ".." -> fromJust $ parent f
    Cd s    -> goToChild f s
    Ls fs   -> modifyTree (flip (foldl addChild) fs) f

data File = File {size :: Int, name :: String} | Folder {name :: String}
    deriving Show
type FTree = Tree File
type FileSys = TreePos Full File

parseFile = (P.string "dir " >> Folder <$> P.getLineS)
    <|> (File <$> P.decimal <*> (P.skipSpace >> P.getLineS))

rootDir :: FileSys
rootDir = fromTree $ pure $ Folder ""

mkDir :: String -> File
mkDir = Folder

isChild :: File -> FTree -> Bool
isChild f = any (((== name f) . name) . rootLabel) . subForest

addChild :: FTree -> File -> FTree
addChild t@Node{subForest = s} f = if isChild f t then t else t{subForest = pure f:s}

goToChild :: FileSys -> String -> FileSys
goToChild f s = case tryFindChild f s of
    Just c -> c
    Nothing -> (`goToChild` s) $ modifyTree (`addChild` Folder s) f

tryFindChild :: FileSys -> String -> Maybe FileSys
tryFindChild f s = firstChild f >>= flip findSibling s

findSibling :: FileSys -> String -> Maybe FileSys
findSibling fs s = case tree fs of
    Node{..} | name rootLabel == s -> Just fs
    _ -> next fs >>= flip findSibling s