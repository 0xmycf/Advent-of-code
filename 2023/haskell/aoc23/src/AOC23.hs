module AOC23 (
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
import           Days.Day1                   (day1)
import           Days.Day2                   (day2)
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
          -- day2  = Solution {day=dayn  2,  partB=const @String "partB 2",  partA=const "partA 2",  common=id}
          day3  = Solution {day=dayn  3,  partB=const @String "partB 3",  partA=const "partA 3",  common=id}
          day4  = Solution {day=dayn  4,  partB=const @String "partB 4",  partA=const "partA 4",  common=id}
          day5  = Solution {day=dayn  5,  partB=const @String "partB 5",  partA=const "partA 5",  common=id}
          day6  = Solution {day=dayn  6,  partB=const @String "partB 6",  partA=const "partA 6",  common=id}
          day7  = Solution {day=dayn  7,  partB=const @String "partB 7",  partA=const "partA 7",  common=id}
          day8  = Solution {day=dayn  8,  partB=const @String "partB 8",  partA=const "partA 8",  common=id}
          day9  = Solution {day=dayn  9,  partB=const @String "partB 9",  partA=const "partA 9",  common=id}
          day10 = Solution {day=dayn 10, partB=const @String "partB 10", partA=const "partA 10", common=id}
          day11 = Solution {day=dayn 11, partB=const @String "partB 11", partA=const "partA 11", common=id}
          day12 = Solution {day=dayn 12, partB=const @String "partB 12", partA=const "partA 12", common=id}
          day13 = Solution {day=dayn 13, partB=const @String "partB 13", partA=const "partA 13", common=id}
          day14 = Solution {day=dayn 14, partB=const @String "partB 14", partA=const "partA 14", common=id}
          day15 = Solution {day=dayn 15, partB=const @String "partB 15", partA=const "partA 15", common=id}
          day16 = Solution {day=dayn 16, partB=const @String "partB 16", partA=const "partA 16", common=id}
          day17 = Solution {day=dayn 17, partB=const @String "partB 17", partA=const "partA 17", common=id}
          day18 = Solution {day=dayn 18, partB=const @String "partB 18", partA=const "partA 18", common=id}
          day19 = Solution {day=dayn 19, partB=const @String "partB 19", partA=const "partA 19", common=id}
          day20 = Solution {day=dayn 20, partB=const @String "partB 20", partA=const "partA 20", common=id}
          day21 = Solution {day=dayn 21, partB=const @String "partB 21", partA=const "partA 21", common=id}
          day22 = Solution {day=dayn 22, partB=const @String "partB 22", partA=const "partA 22", common=id}
          day23 = Solution {day=dayn 23, partB=const @String "partB 23", partA=const "partA 23", common=id}
          day24 = Solution {day=dayn 24, partB=const @String "partB 24", partA=const "partA 24", common=id}
          day25 = Solution {day=dayn 25, partB=const @String "partB 25", partA=const "partA 25", common=id}

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

