
module Main where
import           Control.Monad   (forM_)
import           Data.IntMap     (IntMap)
import qualified Data.IntMap     as IM
import qualified Data.IntSet     as IS
import           Data.IORef      (newIORef, readIORef, writeIORef)
import           Data.List.Split (splitOn)
import           System.IO       (IOMode(WriteMode), hPutStr, withFile)


input :: IO String
input = readFile "../input/day11.txt"

testInput :: IO String
testInput = pure "125 17"

parse :: String -> IntMap Integer
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

rule :: Int -> Integer -> IntMap Integer
rule key value
  | key == 0      = IM.singleton 1 value
  | even(len key) = IM.fromListWith (+) [(fh key, value), (sh key, value)]
  | otherwise     = IM.singleton (key * 2024) value

solution :: IntMap Integer -> Integer -> IntMap Integer
solution mp 0 = mp
solution mp i = solution mp' (i - 1)
  where
    mp' = IM.foldrWithKey (\key value acc ->
                            let ruled = rule key value
                            in IM.unionWith (+) ruled acc)
                          IM.empty
                          mp

summ :: IntMap Integer -> Integer
summ = IM.foldr' (+) 0

uniqueStones :: IntMap Integer -> Int
uniqueStones = IS.size . IM.foldrWithKey' (\key value acc -> IS.insert key acc) IS.empty

main :: IO ()
main = do
  data_ <- parse <$> input
  print $ sum $ solution data_ 25
  print $ sum $ solution data_ 75
  {- ref <- newIORef data_
  withFile "./out.txt" WriteMode $ \fh -> do
    forM_ [1..10_000] $ \(i::Integer) -> do
      mp <- readIORef ref
      let sol = solution mp 1
      hPutStr fh (show . uniqueStones $ sol)
      hPutStr fh "\n"
      writeIORef ref sol -}


