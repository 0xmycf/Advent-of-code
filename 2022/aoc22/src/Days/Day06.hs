module Days.Day06 (day6) where
import           Data.Char     (ord)
import           Data.Foldable (find)
import           Data.IntSet   (IntSet)
import qualified Data.IntSet   as ISet
import           Data.Maybe    (fromJust)
import           Finite        (finite)
import           Solution      (Solution (..))

day6 :: Solution
day6 = Solution {day=finite 5, partA=partA1, partB=partB1, common=id}

common06 :: String -> Int -> Int
common06 input howMany = (+(howMany - 1)) . fst . fromJust . find snd                             -- finding the answer
                          $ [1..] `zip` fmap (\set -> ISet.size set == howMany) (mkSet input [])  -- Creating the List of Bools
  where
  mkSet :: String -> [IntSet] -> [IntSet]
  mkSet []  acc = reverse acc
  mkSet inp acc = mkSet (drop 1 inp) (ISet.fromList (ord <$> take howMany inp) : acc)

partA1 :: String -> Int
partA1 = flip common06 4

partB1 :: String -> Int
partB1 = flip common06 14
