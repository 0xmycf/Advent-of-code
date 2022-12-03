module Days.Day03 (day3) where
import           Data.Char       (isLower, ord)
import           Data.List.Split (chunksOf)
import           Data.Set        (Set)
import qualified Data.Set        as S
import           Finite          (finite)
import           Lib             (both, intersections)
import           Solution        (Solution (..))

day3 :: Solution
day3 = Solution {day=finite 2, partA=partA1, partB=partB1, common=commonDayOne.parseDayOne}

type Rucksack = ([Int], [Int])

parseDayOne  :: String -> [String]
parseDayOne =  lines

commonDayOne :: [String] -> [Rucksack]
commonDayOne = map (both (map toNum) . (splitAt =<< (flip div 2 . length))) -- \xs -> splitAt (length xs `div` 2) xs
  where
  toNum char
    | isLower char = ord char - 96
    | otherwise    = ord char - 38

partA1 :: [Rucksack] -> Int
partA1 = sum . map (S.findMin . uncurry S.intersection . both S.fromList)

partB1 :: [Rucksack] -> Int
partB1= sum  . map mapper . chunksOf 3 . map (S.fromList . uncurry (++))
  where
  mapper :: [Set Int] -> Int
  mapper  = S.findMin . intersections

