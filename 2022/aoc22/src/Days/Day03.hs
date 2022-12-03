{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Days.Day03 (day3) where
import           Data.Char (isLower, ord)
import           Data.List (nub)
import           Data.Set  (Set)
import qualified Data.Set  as S
import           Finite    (finite)
import           Lib       (both, groupN)
import           Solution  (Solution (..))

day3 :: Solution
day3 = Solution {day=finite 2, partA=partA1, partB=partB1, common=commonDayOne.parseDayOne}

type Rucksack = ([Int], [Int])

parseDayOne  :: String -> [String]
parseDayOne =  lines

commonDayOne :: [String] -> [Rucksack]
commonDayOne = map (both (map toNum) . \xs -> splitAt (length xs `div` 2) xs)
  where
  toNum char
    | isLower char = ord char - 96
    | otherwise    = ord char - 38

partA1 :: [Rucksack] -> Int
partA1 = sum . map (head . S.toList . uncurry S.intersection . both S.fromList)

partB1 :: [Rucksack] -> Int
partB1= sum  . map mapper . groupN 3 . map (nub . uncurry (++))
  where
  mapper :: [[Int]] -> Int
  mapper (a:b:c:_) = head . S.toList $ sfl a \/ sfl b \/ sfl c
  sfl = S.fromList

(\/) :: Ord a => Set a -> Set a -> Set a
(\/) = S.intersection

