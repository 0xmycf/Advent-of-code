module Days.Day12 (day12) where
import           Control.Lens (filtered, (&), (.~))
import           Data.Char    (ord)
import           Data.Map     (Map)
import qualified Data.Map     as M
import           Finite       (dayn)
import           Lib          (Point, getNeighbors, gridParser)
import           Linear.V2    (V2)
import           Search       (bfs)
import           Solution     (Solution (..))

day12 :: Solution
day12 = Solution {day=dayn 12, partA=partA1, partB=partB1, common=parseDay11}

parseDay11 :: String -> Map Point Int
parseDay11 = gridParser (\case
                          'S' -> (-1) -- gets changed later again
                          'E' -> 100  -- gets changed later too
                          a   -> ord a - ord 'a'
                        ) . lines

helloNeighbour :: Num n => (n -> n -> Bool) -> Map (V2 Int) n -> V2 Int -> [V2 Int]
helloNeighbour comp grid p =
                    let c = grid M.! p
                        ns = filter (`M.member` grid) $ getNeighbors p
                     in filter (\n -> let nc = grid M.! n in nc `comp` c) ns

common12 :: Int                                      -- ^ -1 for part 1 (starting at S) 100 for partB (starting at E)
            -> (Point -> Bool)                       -- ^ end criterion
            -> (Map Point Int -> Point -> [Point])   -- ^ neighbourhood
            -> Map Point Int                         -- ^ graph
            -> Int
common12 part end n grid = bfs (n grid')
                  end
                  start
                & maybe 0 (pred . length)
  where
        start = head $ M.keys $ M.filter (==part) grid
        grid' = grid & traverse . filtered (==(-1)) .~ 0
                     & traverse . filtered (==100) .~ 25

partA1, partB1 :: Map Point Int -> Int
partA1 grid = common12 (-1) (==end) (helloNeighbour (\nc c -> nc <= (c + 1))) grid
   where end = head $ M.keys $ M.filter (==100) grid

partB1 grid = common12 100 (\v2 -> let num = grid M.! v2 in num == 0) (helloNeighbour (\nc c -> (nc + 1) >= c)) grid


