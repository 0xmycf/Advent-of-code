
module Main where
import           Data.IntMap     (IntMap)
import qualified Data.IntMap     as IM
import           Data.List.Split (splitOn)


input :: IO String
input = readFile "../input/day11.txt"

testInput :: IO String
testInput = pure "125 17"

parse :: String -> IntMap Int
parse input_ = IM.fromList $ map (, 1) split
  where split = read @Int <$> splitOn " " input_

len :: Int -> Int
len i = go i 0
  where
    go i amnt
      | i < 10 = amnt + 1
      | otherwise = go (i `div` 10) (amnt + 1)

fh :: Int  -> Int
fh i = go i (len i `div` 2)
  where
    go i 0    =  i
    go i amnt = go (i `div` 10 ) (amnt - 1)

sh :: Int -> Int
sh i =
  let fh' = fh i
      len' = len fh'
  in i - (fh' * 10 ^ len')

rule :: Int -> Int -> IntMap Int
rule key value
  | key == 0      = IM.singleton 1 value
  | even(len key) = IM.fromListWith (+) [(fh key, value), (sh key, value)]
  | otherwise     = IM.singleton (key * 2024) value

solution :: IntMap Int -> Int -> IntMap Int
solution mp 0 = mp
solution mp i = solution mp' (i - 1)
  where
    mp' = IM.foldrWithKey (\key value acc ->
                            let ruled = rule key value
                            in IM.unionWith (+) ruled acc)
                          IM.empty
                          mp

summ :: IntMap Int -> Int
summ = IM.foldr' (+) 0

main :: IO ()
main = do
  data_ <- parse <$> input
  print $ sum $ solution data_ 25
  print $ sum $ solution data_ 75
