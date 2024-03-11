{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Days.Day4 (day4) where
import           Data.Bits       ((.<<.))
import qualified Data.Bits       ((.<<.))
import qualified Data.List       as L
import           Data.List.Split (splitOn)
import qualified Data.Map.Lazy   as M
import           Data.Maybe      (fromMaybe)
import           Debug
import qualified Debug.Trace
import qualified Debug.Trace     as Trace
import           Finite          (dayn)
import           Prelude         hiding (lookup)
import           Solution        (Solution(..))

day4 :: Solution
day4 = Solution {day=dayn 4, partA=partA1, partB=partB1, common=commonDay4}

{- winning               | you have
  Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
  Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
  Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
  Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
  Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
  Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
-}

-- cards are indexed by 1
data Card
  = Card [Int] [Int]
  -- ^ My numbers
  deriving (Show)
type Problem = [Card] -> Integer

commonDay4 :: String -> [Card]
commonDay4 input = splitAndSort . drop 1 . dropWhile (/=':') <$> lines input
  -- aoc guarantees that this wont crash (unless my assumptions are wrong)
  where splitAndSort (splitOn "|" -> [winning, mine]) =
          let go = map read . filter (not . null) . words
              firstNums = go winning
              secndNums = go mine
           in Card firstNums secndNums

binaryStuff :: Int -> Integer
binaryStuff 0 = 0
binaryStuff x = (1 .<<.) . subtract 1 $ x

partA1 :: Problem
partA1 = sum . fmap (\(Card winning mine) -> let points = length $ winning `L.intersect` mine in binaryStuff points)

-- | This solution is inspired by https://youtu.be/-PX3rUZ1SFM?si=OUa2SyGpRXABfJo0
--   I had the same ideas but I got confused in the process of implementing it
--  
--  The basic idea is to tie the knot, I tied it wrognly
--  Alan Malloy also used list comprehension istead of my wierd and convoluted way (see below)
--
--  I tried to recursively look up the values in the map, insetad of simply assuming the values
--  are already there and i just need to look them up
--
--  I also forgot to add the cards themself to the sum (I incoporated them at the end,
--  but its not show in the code snipped below)
--
--  All in all, this is where Haskell's unique lazyness shines
partB1 :: Problem
partB1 cards = fromIntegral $ M.size ans +  M.foldr (+) 0 ans
  where
    lenCards = fromIntegral $ length cards
    lenghts :: [Int] = fmap (\(Card winning mine) -> length $ winning `L.intersect` mine) cards
    ans :: M.Map Int Int = M.fromList entries
    entries = do
              (icard, len) <- [1..lenCards] `zip` lenghts
              pure (icard, len + sum [ans M.! (icard + i) | i <- [1..len]])

{- 

  where
  -- this is the same --
  lenCards = fromIntegral $ length cards
  lenghts :: [Integer] = fmap (\(Card winning mine) -> fromIntegral . length $  winning `L.intersect` mine) cards
  --       ----       --
  scratchesCache = M.fromList $ [(1::Integer)..lenCards] `zip` lenghts
  -- ans = M.mapWithKey go $ js $ scratchesCache
  ans = tying scratchesCache
  -- go k v = let nums = [k+1..k+v] in fromIntegral $ sum ((\kk -> go kk $ lookup scratchesCache kk) <$> nums)
  lookup mp x = fromMaybe 0 (M.lookup x mp)

tying mp = mp'
  where mp' = M.mapWithKey go mp
        go k =
          let nums = [k+1..k+v]
                 in fromIntegral $ sum {- $ js -} $ v : ((\kk -> go kk $ lookup mp' kk) <$> nums)
        lookup mpp x = fromMaybe (error "not int he map") r

-}
