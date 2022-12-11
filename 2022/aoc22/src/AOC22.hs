module AOC22 (
    registry
  , solveRegistry
  , solveSolution
  , (!)
  ) where
import           Control.Concurrent.Async (mapConcurrently)
import           Control.Monad            (forM_)
import           Data.Text                (pack)
import           Days.Day03               (day3)
import           Days.Day04               (day4)
import           Days.Day05               (day5)
import           Days.Day06               (day6)
import           Days.Day07               (day7)
import           Days.Day08               (day8)
import           Days.Day09               (day9)
import           Days.Day10               (day10)
import           Days.Day11               (day11)
import           Days.DayOne              (day1)
import           Days.DayTwo              (day2)
import           Finite                   (toInt, unwrap)
import           Say                      (say)
import           Solution                 (Solution (..))


-- | Register all days in here -------------------------------------
registry :: Registry
registry = Registry
  [ day1
  , day2
  , day3
  , day4
  , day5
  , day6
  , day7
  , day8
  , day9
  , day10
  , day11
  ]

-- | Sovles and prints out all Soltutions in the provided registry
solveRegistry :: Registry -> IO ()
solveRegistry (Registry rs) = do
                              files <- mapConcurrently readFile [file | x <- [1..length rs], let file = path ++ "day" ++ show x ++ ".txt" ]
                              forM_ rs $ \day' -> do
                                let ix = toInt (day day')
                                say . pack $ solveSolution (files Prelude.!! ix) day'

-- | Sovles and prints out the provided Soltution
solveSolution :: FilePath -> Solution -> String
solveSolution file val@Solution{..} = ansStringBuilder solvea solveb
  where
  parsed = common file
  solvea = partA parsed
  solveb = partB parsed
  ansStringBuilder ansa ansb = "Day " ++ (show . (+1) . unwrap . Solution.day $ val) ++ ": " ++ show ansa ++ ", " ++ show ansb

-- | Helper function to access Solutions by day
-- if the provided int is out of bounds it will default to the first day
(!) :: Registry -> Int -> Solution
(!) (Registry reg) int
                | int > 0 && int < length reg = reg Prelude.!! int
                | otherwise = head reg

-- private -----------------------------------------------------------------------------------------------------------------------

path :: FilePath
path = "./input/"

newtype Registry = Registry [Solution]

