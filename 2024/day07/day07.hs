{-# LANGUAGE BlockArguments, ViewPatterns #-}

{-

ghc -O2 ./day07.hs
________________________________________________________
Executed in    1.21 secs      fish           external
   usr time  848.72 millis  118.00 micros  848.60 millis
   sys time   31.61 millis  676.00 micros   30.93 millis

-}

module Main where

import           Control.Monad   (guard)
import           Data.List       (foldl', nub)
import           Data.List.Split (splitOn)
import           Data.Map.Lazy   (Map)
import qualified Data.Map.Lazy   as Map
import           System.Exit     (exitSuccess)

input :: IO [String]
input = lines <$> readFile "../input/day07.txt"

type Entry  = (Int, Int) -- the second int is its length

type Input = [(Int, [Entry])]

parse :: [String] -> [(Int, [Entry])]
parse lines = [ (first, rest) | line <- lines
              , let [read @Int->first, parseRest->rest] = splitOn ":" line ]
  where parseRest xs = let s = filter (not . null) $ splitOn " " xs in fmap (\x -> (read @Int x, length x)) s

-- use these instead to do part 1
opsA :: Num a => [a -> a -> a]
opsA = [(+), (*)]

-- use these for part 2
ops :: [(Int, b1) -> (Int, Int) -> (Int, b2)]
ops = [plus, times, (|||)]
  where
    plus (a,_) (b,_) = (a + b, undefined)
    times (a,_) (b,_) = (a * b, undefined)
    (a,_) ||| (b,blen) = ((a * (lens Map.! blen)) + b, undefined)
    lens :: Map Int Int
    lens = Map.fromList $ take 20 $ [1..] `zip` iterate (*10) 10

solve :: Input -> Int
solve lines = sum  do
  (target, x:xs) <- lines
  let foo = foldl' (\acc val -> fmap (`flip` val) ops <*> acc) [x] xs
  guard (target `elem` fmap fst foo)
  pure target

main :: IO ()
main = do
  in_ <- parse <$> input
  print $ solve in_

