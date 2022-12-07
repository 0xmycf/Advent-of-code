module Lib where

import           Data.Bits             (Bits (shiftL, (.|.)))
import           Data.Char             (ord)
import           Data.Foldable         (toList)
import           Data.Functor.Identity (Identity)

import           Data.Map              (Map)
import           Data.Set              (Set)
import qualified Data.Set              as Set

import           Data.Bifunctor        (Bifunctor, bimap)
import           Data.List             (group, sort)
import qualified Data.List             as List
import qualified Data.Map              as Map
import           Linear                (V2 (V2))
import qualified Text.Parsec           as Parsec

type Point = V2 Int

type IdentityParser stream userstate returntype = Parsec.ParsecT stream userstate Identity returntype
type Parser state returntype = Parsec.ParsecT String state Identity returntype

getLines :: FilePath -> IO [String]
getLines = fmap lines . readFile

-- | Shortcut for parsing a string with Parsec
parse :: Parsec.Stream s Identity t => Parsec.Parsec s () a -> s -> Either Parsec.ParseError a
parse rule = Parsec.parse rule "(source)"

-- | maps indexed
mapIdx :: (a -> Int -> b) -> [a] -> [b]
mapIdx f l = zipWith f l [0..]

-- | parses a list like 1,2,3,4,5 to [1,2,3,4,5]
commaListParser :: String -> IO (Either Parsec.ParseError [Int])
commaListParser s = fmap (fmap read) . parse (Parsec.sepBy (Parsec.many Parsec.digit) (Parsec.char ',')) <$> readFile s

-- | Parses a grid
--   Grid is indexed like a matrix
--   Grid looks like this:
--   (1,1) (1,2) (1,3) ...
--   (2,1) (2,2) (2,3) ...
--   ...
gridParser :: (Char -> a) -> [String] -> Map Point a
gridParser f = Map.fromList . concat . mapIdx (\a idx1 -> mapIdx (\b idx2 -> (V2 (idx1 + 1) (idx2 + 1), f b)) a)

-- | Creates a bitmask out of the char using the ord function
-- | ord 'a' = 97
maskChar :: Integer -> Char -> Integer
maskChar acc c = acc .|. 1 `shiftL` (ord c - 97)

-- | Frequency Map
frequencyMap :: (Foldable f, Ord a) => f a -> Map a Int
frequencyMap = Map.fromListWith (+) . map (\x -> (head x, length x)) . group . sort . toList

-- | Converts a binary string to a decimal integer
-- | Example: 10101
-- | 2^0 * 1 + 2^1 * 0 + 2^2 * 1 + 2^3 * 0 + 2^4 * 1 = 1 + 4 + 16 = 21
-- | sum_{i=0}{j-1}(2^i * x)
-- |  where
-- |   j is the length bitstring
-- |   x is the ith digit of the bitstring
-- | Figured the correct syntax with a bit of help from here:
-- | https://stackoverflow.com/questions/44217310/convert-binary-string-to-integer-value-using-first-order-functions
binToDec :: String -> Int
binToDec xs = go (0 :: Integer) (reverse xs)
    where
        go _ []      = 0
        go i (x:xs') = (2^i * read [x]) + go (i+1) xs'

-- | gets parallel (to x/y axis) perpendicular vectors to the input Coordinate
getNeighbors :: V2  Int -> [V2  Int]
getNeighbors (V2 a b) = [V2 a (b-1), V2 (a+1) b, V2 a (b+1), V2 (a-1) b]

-- | gets neighbors parallel to x/y axis as well as the vectors in 45 degree angle relative to those.
getAllNeighbs :: V2  Int -> [V2  Int]
getAllNeighbs (V2 a b) = [V2 a (b-1), V2 (a-1) (b-1) , V2 (a+1) b, V2 (a+1) (b+1), V2 a (b+1), V2 (a-1) (b+1), V2 (a-1) b, V2 (a+1) (b-1)]

-- | gets all Points in a 3x3 field, where the center is the input Point
getNine :: V2 Int -> [V2 Int]
getNine (V2 a b) = [V2 x y | x <- [a-1, a, a+1], y <- [b-1, b, b+1]]

updiv :: Integral a => a -> a -> a
updiv a b = if a `mod` b == 0 then a `div` b else (a `div` b) + 1

-- | unsafe split of a list into chunks of provided size
-- throws an error if n < 1
groupN :: Int -> [a] -> [[a]]
groupN _ [] = []
groupN n l
  | n > 0     = take n l : groupN n (drop n l)
  | otherwise = error "Negative or zero n"

-- | safely splits a list into chunks of the provided Int = n
-- returns Nothing if the list is not evenly divisible by n or n < 1
groupNs :: Int -> [a] -> Maybe [[a]]
groupNs n l
  |  n > 0
  && length l `mod` n == 0  = Just $ take n l : groupN n (drop n l)
  | otherwise               = Nothing

-- | converts a list of exactly 2 elements into a tuple
-- returns Nothing if the list is bigger or smaller than 2
tuple2 :: [a] -> Maybe (a, a)
tuple2 [a,b] = Just (a,b)
tuple2 _     = Nothing

-- | converts a list of exactly 3 elements into a tuple
-- returns Nothing if the list is bigger or smaller than 3
tuple3 :: [a] -> Maybe (a, a, a)
tuple3 [a,b,c] = Just (a,b,c)
tuple3 _       = Nothing

-- | shorthand for bimap f f
-- same as join bimap
both :: Bifunctor p => (c -> d) -> p c c -> p d d
both f = bimap f f

-- | Same as @Set.unions@ for Sets
intersections :: Ord a => [Set a] -> Set a
intersections = List.foldl1' Set.intersection

-- | Inline  @Set.intersection@
(/\) :: Ord a => Set a -> Set a -> Set a
(/\) = Set.intersection

-- | Inline  @Set.union@
(\/) :: Ord a => Set a -> Set a -> Set a
(\/) = Set.union
