module Days.DayOne (day1) where
import           Data.List (sort)
import           Finite    (finite)
import           Solution  (Solution (..))

day1 :: Solution
day1 = Solution {day=finite 0, partA=show.partA1, partB=show.partB1, common=commonDayOne.parseDayOne}

parseDayOne  :: String -> [[Int]]
parseDayOne input = go splitted [] []
  where
  splitted = lines input
  go ("":xs) tmp acc = go xs [] (tmp : acc)
  go (x:xs) tmp acc  = go xs (read x : tmp) acc
  go [] _ acc        = acc

commonDayOne :: [[Int]] -> [Int]
commonDayOne = fmap sum

partA1 :: [Int] -> Int
partA1 = maximum

partB1 :: [Int] -> Int
partB1 xs = sum $ take 3 sortedByMost
  where
  sortedByMost = reverse $ sort xs
