module Days.Day04 (day4) where
import           Data.Char       (digitToInt)
import           Data.List       (nub)
import           Data.List.Split (splitOn)
import qualified Data.Map        as Map
import           Data.Maybe      (fromJust)
import           Lib             (both, frequencyMap, tuple2)
import           Solution        (Solution (..))

day4 :: Solution
day4 = Solution {day=3, partA=flip _partA (\asS -> (length . nub) asS < 6), partB=_partB, common=_common}
                  --

_common :: String -> (Int, Int)
_common = both read . fromJust . tuple2 . splitOn "-"

_partB :: (Int, Int) -> Int
_partB t = _partA t (not . Map.null . Map.filter (== 2)  . frequencyMap)

_partA :: (Int, Int) -> (String -> Bool) -> Int
_partA (from, to) checkDouble = length [x | x <- [from .. to], checkDouble' x, checkIncreasing x]
  where
  checkDouble' :: Int -> Bool
  checkDouble' x = let asS = show x
                  in checkDouble asS
  checkIncreasing :: Int -> Bool
  checkIncreasing x = let asS = show x
                      in and $ zipWith (\a b -> (digitToInt b - digitToInt a) >= 0) asS (drop 1 asS)
