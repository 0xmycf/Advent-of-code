module Days.Day18 (day18) where
import           Control.Lens    (Each (each), (^..))
import           Data.Foldable   (foldl')
import           Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import           Data.Monoid     (All (All, getAll))
import           Data.Set        (Set)
import qualified Data.Set        as S
import           Finite          (dayn)
import           Lib             (getNeighbors3)
import           Linear          (V3 (V3))
import qualified Linear.V3       as V3
import           Search          (bfsParents)
import           Solution        (Solution (..))

day18 :: Solution
day18 = Solution {day=dayn 18, partA=partA1, partB=partB1, common=parseDay18}

-- Parsing and datatypes ----------------------------------------------------

parseDay18 :: String -> [V3 Int]
parseDay18 = ((toV3 . splitOn ",") <$>) . lines
  where toV3 [x,y,z] = read <$> V3 x y z
        toV3 _       = error "unreachable!"

-- Solving ------------------------------------------------------------------

common18 :: (Num a, Foldable t) => (V3 a -> Bool) -> t (V3 a) -> Int
common18 predicate = foldr folder 0
  where folder value acc = acc + 6 - length (filter predicate nbs)
          where nbs = getNeighbors3 value

partA1, partB1 :: [V3 Int] -> Int
partA1 v3s = common18 (`elem` v3s) v3s
partB1 v3s = common18 (\v -> v `elem` v3s || v `S.member` allEnclosedV3) v3s
  where allEnclosedV3 = foldl' enclosed S.empty (concatMap getNeighbors3 v3s)

        enclosed :: S.Set (V3 Int) -> V3 Int -> Set (V3 Int)
        enclosed seen val = add $ bfsParents getN isEnd val

          where add (Just _,_)   = seen
                add (Nothing, p) = S.union seen (M.keysSet p)

                getN = filter (\n -> not ((n `S.member` seen) || (n `elem` v3s))) . getNeighbors3

                isEnd = not . isEnclosed

                isEnclosed (V3 x y z) = getAll $ foldMap (All . inDirection) [toEnum @Direction i | i <- [0..2]]

                  -- DRY ? Faster ?
                  -- I could check if a point has a neighbour in the same direction that is
                  -- encapsulated, if yes than that point is encapsulated too. But It runs fast enough.
                  where
                  inDirection Up       = let ls =  [ v | v@(V3 a b _) <- v3s, a == x, b == y]
                                          in 2 <= length ls && inBetween Up ls
                  inDirection Lefd     = let ls = [ v | v@(V3 a _ c) <- v3s, a == x, c == z]
                                          in 2 <= length ls && inBetween Lefd ls
                  inDirection Forward  = let ls = [ v | v@(V3 _ b c) <- v3s, b == y, c == z]
                                          in 2 <= length ls && inBetween Forward ls

                  inBetween :: Direction -> [V3 Int] -> Bool
                  inBetween dir ls         = lsmin < xyz && xyz < lsmax
                    where lsmax = maximum $ ls ^.. each . xyz_
                          lsmin = minimum $ ls ^.. each . xyz_
                          (xyz_ , xyz)  = case dir of
                                  Up      -> (V3._z, z)
                                  Lefd    -> (V3._y, y)
                                  Forward -> (V3._x, x)

{-

  For partb we need to check if all neighbours are enclosed.
    If one is not enclosed, all are not enclosed
    We can do this by doing a bfs, where the end criterion is: for a given vector v, it has no wall in sight in any direction.
    If we find a path, then we know its not enclosed

  Pseudo

  for x in input                           -- foldr input
    seen = set()                           -- handled by the search

    for n in getNeighbour x                -- Neighbourhood
      if n in seen or in input: continue   --

      if isEnclosed n                      -- isEnd
        seen = seen + n                    --

    return seen                            -- Parents


-}
-- Lefd != Prelude.Left
data Direction = Up | Lefd | Forward
  deriving (Enum)
