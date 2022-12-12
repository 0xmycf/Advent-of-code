module Days.Day04 (day4) where
import           Data.List.Split (splitOn)
import           Data.Maybe      (fromJust)
import           Finite          (dayn)
import           Lib             (both, tuple2)
import           Solution        (Solution (..))

day4 :: Solution
day4 = Solution {day=dayn 4, partA=partA1, partB=partB1, common=parseDay04}

data Range = Range
              { _from :: Int
              , _to   :: Int
              } deriving (Show, Eq)

-- | Checks if one of the ranges fully contains the other
--       0-----|--|-----|--------|---
--            f1  f2    t2       t1
--
--       0-----|--|-----|--------|---
--            f2  f1    t2       t1
--
--       0--------|-----|-----|--|---
--                f2    t2   f1  t1
fullyContains :: Range -> Range -> Bool
fullyContains (Range f1 t1) (Range f2 t2) = case compare f1 f2 of
                                        LT -> t2 <= t1
                                        GT -> t2 >= t1
                                        EQ -> True

-- | Checks if one of the ranges partly contains the other
contains :: Range -> Range -> Bool
contains (Range f1 t1) (Range f2 t2) = case compare f1 f2 of
                                        LT -> f2 <= t1
                                        GT -> f1 <= t2
                                        EQ -> True

-- dont ask me why but this fails at runtime
-- contains (Range f1 t1) (Range f2 t2)
--                       | f1 < f2  = t2 <= t1
--                       | f1 > f1  = t2 >= t1
--                       | f1 == f2 = True

range :: Int -> Int -> Range
range a b | a <= b     = Range a b
          | otherwise = error "cant construct range backwards"

parseDay04  :: String -> [(Range, Range)]
parseDay04 input = let t = both (uncurry range . both (read @Int) . fromJust . tuple2 . splitOn "-" ). fromJust . tuple2 . splitOn "," <$> lines input
                    in t

common04 :: (Foldable t, Enum a1) => (a2 -> b -> a1) -> t (a2, b) -> Int
common04 f = foldr (\val acc -> fromEnum (uncurry f val) + acc) 0

partA1 :: [(Range, Range)] -> Int
partA1 = common04 fullyContains

partB1 :: [(Range, Range)] -> Int
partB1 = common04 contains
