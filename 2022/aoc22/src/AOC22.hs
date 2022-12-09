{-# OPTIONS_GHC -Wno-unused-imports #-}
module AOC22 (
    registry
  , solveRegistry
  , solveSolution
  , (!)
  ) where
import           Control.Applicative (Applicative (liftA2))
import           Data.Foldable       (foldrM)
import           Days.Day03          (day3)
import           Days.Day04          (day4)
import           Days.Day05          (day5)
import           Days.Day06          (day6)
import           Days.Day07          (day7)
import           Days.Day08          (day8)
import           Days.Day09          (day9)
import           Days.DayOne         (day1)
import           Days.DayTwo         (day2)
import           Finite              (Finite, finite, unwrap)
import           Solution            (Solution (..))
import           Text.Read           (Lexeme (String))


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
  ]

-- | Sovles and prints out all Soltutions in the provided registry
solveRegistry :: Registry -> IO ()
solveRegistry (Registry rs) = foldrM go "" rs >>= putStr
  where
  go :: Solution -> String -> IO String
  go sol acc = fmap (acc ++) (solveSolution sol)

-- | Sovles and prints out the provided Soltution
solveSolution :: Solution -> IO String
solveSolution val@Solution{..} = liftA2 ansStringBuilder solvea solveb
  where
  newline = "\n" :: String
  file = readFile $ path ++ ( "day" ++ (show . (+1) . unwrap . Solution.day $ val) ++ ".txt")
  parsed = common <$> file
  solvea = partA <$> parsed
  solveb = partB <$> parsed
  ansStringBuilder ansa ansb = "Day " ++ (show . (+1) . unwrap . Solution.day $ val) ++ ": " ++ show ansa ++ ", " ++ show ansb ++ newline

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

