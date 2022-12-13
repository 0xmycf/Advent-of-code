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

helloNeighbour :: Map Point Int -> V2 Int -> [V2 Int]
helloNeighbour grid p =
                    let c = grid M.! p
                        ns = filter (`M.member` grid) $ getNeighbors p
                     in filter (\n -> let nc = grid M.! n in nc <= c + 1) ns

common12 :: (Maybe [Point] -> b) -> Point -> Map Point Int -> b
common12 f start grid = bfs (helloNeighbour grid')
                  (==end)
                  start
                & f
  where end   = head $ M.keys $ M.filter (==100) grid
        grid' = grid & traverse . filtered (==(-1)) .~ 0
                     & traverse . filtered (==100)   .~ 25

partA1, partB1 :: Map Point Int -> Int
partA1 grid = common12 (maybe 0 (pred . length)) start grid
  where start = head $ M.keys $ M.filter (==(-1)) grid

partB1 grid = minimum $ fmap (\p -> common12 (maybe 10_000 (pred . length)) p grid) as
  where as = M.keys $ M.filter (==0) grid


