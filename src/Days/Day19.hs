module Days.Day19 where

import Util.Util
import Util.Parsing ( Parser )
import qualified Util.Parsing as P

import qualified Data.Map as M
import Data.List ( maximumBy )
import Data.Ord ( comparing )

import qualified Program.RunDay as R (runDay, Day)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

type Input = [Blueprint]
type OutputA = Int
type OutputB = ()

inputParser :: Parser Input
inputParser = P.many' parseBP

-- partA :: Input -> OutputA
partA = map (pure .> iterate oneMinute .> (!! 3) .> map score)
    -- .> map mats

partB :: Input -> OutputB
partB = undefined

(+=), (-=) :: (Ord k, Num a) => M.Map k a -> (k, a) -> M.Map k a
m += (a, b) = M.insertWith (+) a b m
m -= (a, b) = M.insertWith (+) a (-b) m
m ? a = M.findWithDefault 0 a m

data Material = Ore | Clay | Obsidian | Geode deriving (Eq, Ord, Show)
matList = [Ore, Clay, Obsidian, Geode]
data Blueprint = BP
    { bpID :: Int
    , cost :: M.Map Material [(Material, Int)]
    , mats :: M.Map Material Int
    , bots :: M.Map Material Int
    } deriving Show

score :: Blueprint -> Int
score = do
    g <- mats .> (? Geode)
    i <- bpID
    return $ g * i

oneMinute :: [Blueprint] -> [Blueprint]
oneMinute = concatMap makeBots .> map getMaterials

makeBots :: Blueprint -> [Blueprint]
makeBots bp@BP{..} = bp : [BP bpID cost mats' bots'
    | mat <- matList
    , let cs = cost M.! mat
    , all (\(m, c) -> mats ? m >= c) cs
    , let mats' = foldl (-=) mats cs
    , let bots' = bots += (mat, 1)]

getMaterials :: Blueprint -> Blueprint
getMaterials bp@BP{..} = bp{mats = foldl (+=) mats $ M.assocs bots}

parseMat :: Parser Material
parseMat = Ore <$ "ore" <|> Clay <$ "clay" <|> Obsidian <$ "obsidian" <|> Geode <$ "geode"

parseCost :: Parser (Material, Int)
parseCost = flip (,) <$> P.token P.decimal <*> P.token parseMat

parseCosts :: Parser (M.Map Material [(Material, Int)])
parseCosts = fmap M.fromList $ P.many' $ do
    mat <- P.token "Each" *> parseMat <* P.token "robot costs"
    costs <- P.split parseCost (P.token "and")
    (mat, costs) <$ P.token "."

parseBP :: Parser Blueprint
parseBP = do
    i <- "Blueprint " *> P.decimal <* ":"
    costs <- parseCosts
    return $ BP i costs M.empty (M.singleton Ore 1)