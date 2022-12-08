{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module Days.Day08 (day8) where
import           Data.Char       (digitToInt)
import           Data.List       (unfoldr)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import           Finite          (finite)
import           Lib             (Point, gridParser, takeWhileOneMore)
import           Linear          (V2 (..))
import           Solution        (Solution (..))

day8 :: Solution
day8 = Solution {day=finite 7, partA=partA1, partB=partB1, common=parseDay08}

type TreeGrid = Map Point Int

parseDay08 :: String -> TreeGrid
parseDay08 (lines->input) = let len = length input -- should be 99
                             in gridParser digitToInt input
                                `Map.union` -- left biased => no overriding of keys which are already in there
                                Map.fromList [(V2 x y, -1) | x <- [-1..len], y <- [-1..len]] -- we apply padding to make it easier later on
                                `Map.union`
                                Map.fromList [(V2 1000 1000, len)] -- shh, dont talk about this

partA1, partB1 :: TreeGrid -> Int
partA1 mp = length . Map.filterWithKey go $ mp
  where toBeChecked = Set.fromList [V2 x y | x <- [0..len - 1], y <- [0..len - 1]] :: Set.Set (V2 Int)
        go key _     | not $ key `Set.member` toBeChecked =  False -- dont care about the ones we added as padding
        go key value = any (all (<value)) (mkValues mp key)
        len = mp Map.! V2 1000 1000

mkValues :: Map (V2 Int) a -> V2 Int -> [[a]]
mkValues mp key = let fns = fmap getValues pnts  -- [unfoldr 1 0, unfoldr 0 1, unfoldr -1 0, unfoldr 0 -1]
                   in fmap ($ key) fns              -- [values for going down, values for going right, values for going up, values for going left]
  where
  getValues x key' = tail $ unfoldr (\b -> let h = Map.lookup b mp in (, x + b) <$> h) key' -- tail to remove the inital value
  pnts = [V2 1 0, V2 0 1, V2 (-1) 0, V2 0 (-1)] :: [V2 Int] -- helper for the getValues function


partB1 mp = maximum . Map.mapWithKey go $ mp
  where toBeChecked = Set.fromList [V2 x y | x <- [1..len - 2], y <- [1..len - 2]] :: Set.Set (V2 Int)
        go key _     | not $ key `Set.member` toBeChecked =  0 -- dont care about the ones on the outer edge, bcs they will always be 0
        go key value = let values = mkValues mp key
                        in product $ fmap (length . takeWhileOneMore (\x -> x<value && x /= -1) .init ) values
        len = mp Map.! V2 1000 1000

