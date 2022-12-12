module Days.DayOne (day1) where
import           Data.List (sort)
import           Finite    (dayn)
import           Solution  (Solution (..))

day1 :: Solution
day1 = Solution {day=dayn 1, partA=partA1, partB=partB1, common=commonDayOne.parseDayOne}

parseDayOne  :: String -> [[Int]]
parseDayOne input = go splitted []
  where
  splitted = lines input
  go ("":xs) tmp = tmp : go xs []
  go (x:xs) tmp  = go xs (read x : tmp)
  go [] t        = [t]

commonDayOne :: [[Int]] -> [Int]
commonDayOne = fmap sum

partA1 :: [Int] -> Int
partA1 = maximum

partB1 :: [Int] -> Int
partB1 xs = sum $ take 3 sortedByMost
  where
  sortedByMost = reverse $ sort xs
