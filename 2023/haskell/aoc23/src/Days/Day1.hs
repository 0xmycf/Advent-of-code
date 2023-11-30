module Days.Day1 (day1) where
import           Finite    (dayn)
import           Solution  (Solution(..))

day1 :: Solution
day1 = Solution {day=dayn 1, partA=partA1, partB=partB1, common=commonDayOne}

commonDayOne :: String -> String
commonDayOne = id

partA1 :: String -> String
partA1 = const "to be released"

partB1 :: String -> String
partB1 = const "to be released"
