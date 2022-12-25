module AOC22 (
    registry
  , solveRegistry
  , solveSolution
  , (!)
  , len
  ) where
import           Control.Concurrent.Async    (mapConcurrently)
import           Control.DeepSeq             (force)
import           Control.Monad               (forM_)
import           Control.Parallel.Strategies (parList, rdeepseq, using)
import           Data.Text                   (pack)
import           Days.Day03                  (day3)
import           Days.Day04                  (day4)
import           Days.Day05                  (day5)
import           Days.Day06                  (day6)
import           Days.Day07                  (day7)
import           Days.Day08                  (day8)
import           Days.Day09                  (day9)
import           Days.Day10                  (day10)
import           Days.Day11                  (day11)
import           Days.Day12                  (day12)
import           Days.Day13                  (day13)
import           Days.Day14                  (day14)
import           Days.Day15                  (day15)
import           Days.Day18                  (day18)
import           Days.Day21                  (day21)
import           Days.Day25                  (day25)
import           Days.DayOne                 (day1)
import           Days.DayTwo                 (day2)
import           Finite                      (toInt, unwrap)
import           Say                         (say)
import           Solution                    (Solution (..))


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
  , day12
  , day13
  , day14
  , day15
  , day16
  , day17
  , day18
  , day19
  , day20
  , day21
  , day22
  , day23
  , day24
  , day25
  ] where --                        --
          day16 = error "not done yet"
          day17 = error "not done yet"
          day19 = error "not done yet"
          day20 = error "not done yet"
          day22 = error "not done yet"
          day23 = error "not done yet"
          day24 = error "not done yet"

-- | Sovles and prints out all Soltutions in the provided registry
solveRegistry :: Registry -> IO ()
solveRegistry (Registry rs) = do
                              files <- mapConcurrently readFile [file | x <- [1..length rs], let file = path ++ "day" ++ show x ++ ".txt" ]
                              let rs' = map (\day' -> let ix = toInt (day day') in pack $! solveSolution (files Prelude.!! ix) day') rs `using` parList rdeepseq
                              forM_ (force rs') $ \day' -> do
                                say day'

-- | Sovles and prints out the provided Soltution
solveSolution :: String -> Solution -> String
solveSolution file val@Solution{..} = mkResult solvea solveb
  where
  parsed = common file
  solvea = partA parsed
  solveb = partB parsed
  mkResult ansa ansb = "Day " ++ (show . (+1) . unwrap . Solution.day $ val) ++ ": " ++ show ansa ++ ", " ++ show ansb

-- | Helper function to access Solutions by day
-- If the provided int is out of bounds it will default to the first day
--
-- This function is unsafe and will crash if used on an empty registry
(!) :: Registry -> Int -> Solution
(!) (Registry reg) int
                | int > 0 && int < length reg = reg Prelude.!! int
                | otherwise = head reg

len :: Registry -> Int
len (Registry reg) = length reg

-- private -----------------------------------------------------------------------------------------------------------------------

path :: FilePath
path = "./input/"

newtype Registry = Registry [Solution]

