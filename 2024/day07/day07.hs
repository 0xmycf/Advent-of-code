{-# LANGUAGE BlockArguments, ViewPatterns #-}

module Main where

import           Control.Monad   (guard)
import           Data.List       (foldl', nub)
import           Data.List.Split (splitOn)
import           Data.Map.Lazy   (Map)
import qualified Data.Map.Lazy   as Map

input :: IO [String]
input = lines <$> readFile "../input/day07.txt"

type Input = [(Int, [Int])]

parse :: [String] -> [(Int, [Int])]
parse lines = [ (first, rest) | line <- lines
              , let [read @Int->first, parseRest->rest] = splitOn ":" line ]
  where parseRest xs = let s = filter (not . null) $ splitOn " " xs in fmap (read @Int) s

-- use these instead to do part 1
opsA :: Num a => [a -> a -> a]
opsA = [(+), (*)]

-- use these for part 2
ops :: [Int -> Int -> Int]
ops = [(+), (*), (|||)]
  where
    (show->a) ||| (show->b) = read (a <> b)

solve :: Input -> Int
solve lines = sum  do
  (target, x:xs) <- lines
  let foo = foldl' (\acc val -> fmap (`flip` val) ops <*> acc) [x] xs
  guard (target `elem` foo)
  pure target

main :: IO ()
main = do
  in_ <- parse <$> input
  print $ solve in_

