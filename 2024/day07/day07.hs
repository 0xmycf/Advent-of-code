{-# LANGUAGE BlockArguments, ViewPatterns #-}

module Main where

import           Control.Monad   (guard)
import           Data.List       (foldl', nub)
import           Data.List.Split (splitOn)
import           Data.Map        (Map)
import qualified Data.Map        as Map

input :: IO [String]
input = lines <$> readFile "../input/day07.txt"

type Input = [(Int, [Int])]

parse :: [String] -> [(Int, [Int])]
parse lines = [ (first, rest) | line <- lines 
              , let [read @Int->first, parseRest->rest] = splitOn ":" line ]
  where parseRest xs = let s = filter (not . null) $ splitOn " " xs in fmap (read @Int) s

ops :: Num a => [a -> a -> a]
ops = [(+), (*)]

solve :: Input -> Int
solve lines = sum  do
  (target, x:xs) <- lines
  let foo = foldl' (\acc val -> fmap ($ val) ops <*> acc) [x] xs
  guard (target `elem` foo)
  pure target

main :: IO ()
main = do
  in_ <- parse <$> input
  print $ solve in_

