module Main where

import Options.Applicative
import Data.Map (Map, fromList, mapWithKey, (!?))
import Text.Printf

-- Day imports
import Program.RunDay (Day)
import qualified Days.Day01 as D01 (runDay)
import qualified Days.Day02 as D02 (runDay)
import qualified Days.Day03 as D03 (runDay)
import qualified Days.Day04 as D04 (runDay)
import qualified Days.Day05 as D05 (runDay)
import qualified Days.Day06 as D06 (runDay)
import qualified Days.Day07 as D07 (runDay)
import qualified Days.Day08 as D08 (runDay)
import qualified Days.Day09 as D09 (runDay)
import qualified Days.Day10 as D10 (runDay)
import qualified Days.Day11 as D11 (runDay)
import qualified Days.Day12 as D12 (runDay)
import qualified Days.Day13 as D13 (runDay)
import qualified Days.Day14 as D14 (runDay)
import qualified Days.Day15 as D15 (runDay)
import qualified Days.Day16 as D16 (runDay)
import qualified Days.Day17 as D17 (runDay)
import qualified Days.Day18 as D18 (runDay)
import qualified Days.Day19 as D19 (runDay)
import qualified Days.Day20 as D20 (runDay)
import qualified Days.Day21 as D21 (runDay)
import qualified Days.Day22 as D22 (runDay)
import qualified Days.Day23 as D23 (runDay)
import qualified Days.Day24 as D24 (runDay)
import qualified Days.Day25 as D25 (runDay)

data Days
    = AllDays
    | OneDay Int

dayParser :: Parser Days
dayParser = OneDay <$> day <|> allDays where
    day = option auto $ long "day" <> short 'd' <> help "Solve a single day"
    allDays = flag' AllDays $ long "all-days" <> short 'a' <> help "Solve all 25 days"

days :: Map Int (Day, String)
days = fromList $ zip [1 .. ] [
    (D01.runDay, "data/Day01.txt"),
    (D02.runDay, "data/Day02.txt"),
    (D03.runDay, "data/Day03.txt"),
    (D04.runDay, "data/Day04.txt"),
    (D05.runDay, "data/Day05.txt"),
    (D06.runDay, "data/Day06.txt"),
    (D07.runDay, "data/Day07.txt"),
    (D08.runDay, "data/Day08.txt"),
    (D09.runDay, "data/Day09.txt"),
    (D10.runDay, "data/Day10.txt"),
    (D11.runDay, "data/Day11.txt"),
    (D12.runDay, "data/Day12.txt"),
    (D13.runDay, "data/Day13.txt"),
    (D14.runDay, "data/Day14.txt"),
    (D15.runDay, "data/Day15.txt"),
    (D16.runDay, "data/Day16.txt"),
    (D17.runDay, "data/Day17.txt"),
    (D18.runDay, "data/Day18.txt"),
    (D19.runDay, "data/Day19.txt"),
    (D20.runDay, "data/Day20.txt"),
    (D21.runDay, "data/Day21.txt"),
    (D22.runDay, "data/Day22.txt"),
    (D23.runDay, "data/Day23.txt"),
    (D24.runDay, "data/Day24.txt"),
    (D25.runDay, "data/Day25.txt")]

performDay :: Days -> IO ()
performDay = \case
    OneDay d -> case days !? d of
        Nothing -> putStrLn "Invalid day given"
        Just (day, i) -> do
            putStrLn $ printf "\nDay %02d" d
            day i
    AllDays -> do
        sequence_ $ mapWithKey (\day (d, i) -> putStrLn (printf "\n%02d" day) >> d i) days

main :: IO ()
main = execParser dayP >>= performDay where
    dayP = info (dayParser <**> helper) fullDesc