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
import           Control.Parallel            (par, pseq)
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
import           Days.Day16                  (day16)
import           Days.Day18                  (day18)
import           Days.Day19                  (day19)
import           Days.Day21                  (day21)
import           Days.Day25                  (day25)
import           Days.DayOne                 (day1)
import           Days.DayTwo                 (day2)
import           Finite                      (dayn, toInt, unwrap)
import           Say                         (say)
import           Solution                    (Solution(..))
import           System.IO.Error             (catchIOError)

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
          day17 = Solution {day=dayn 17, partB=const @String "partB 17", partA=const "partA 17", common=id}
          day20 = Solution {day=dayn 20, partB=const @String "partB 20", partA=const "partA 20", common=id}
          day22 = Solution {day=dayn 22, partB=const @String "partB 22", partA=const "partA 22", common=id}
          day23 = Solution {day=dayn 23, partB=const @String "partB 23", partA=const "partA 23", common=id}
          day24 = Solution {day=dayn 24, partB=const @String "partB 24", partA=const "partA 24", common=id}

-- | Sovles and prints out all Soltutions in the provided registry
solveRegistry :: Registry -> IO ()
solveRegistry (Registry rs) = do
                              files <- mapConcurrently
                                        (\f -> readFile f `catchIOError` const (pure ""))
                                        [file | x <- [1..length rs], let file = path ++ "day" ++ show x ++ ".txt" ]
                              let rs' = map (\day' -> let ix = toInt (day day') in pack $! solveSolution (files Prelude.!! ix) day') rs `using` parList rdeepseq
                              forM_ (force rs') $ \day' -> do
                                say day'

-- | Sovles and prints out the provided Soltution
solveSolution :: String -> Solution -> String
solveSolution file val@Solution{..} = mkResult solvea solveb
  where
  parsed = common file
  solvea = partA parsed
  solveb = solvea `par` partB parsed
  mkResult ansa ansb = solveb `pseq` "Day " ++ (show . (+1) . unwrap . Solution.day $ val) ++ ": " ++ show ansa ++ ", " ++ show ansb

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

newtype Registry
  = Registry [Solution]

