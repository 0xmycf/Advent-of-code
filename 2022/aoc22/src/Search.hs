{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
module Search (bfs) where
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Sequence   (Seq ((:<|)))
import qualified Data.Sequence   as Seq
import           Data.Set        (Set)
import qualified Data.Set        as S
import           Lib             (Point)

-- | Not so optimized bfs
bfs :: (Point -> [Point])    -- ^ neighbourhood
    -> (Point -> Bool)       -- ^ end point criterion
    -> Point                 -- ^ starting point
    -> Maybe [Point]         -- ^ returns the paths cost and the path if one was found
bfs neighbourhood end start = goBack $ runBFS M.empty (Seq.singleton start) (S.singleton start)
  where runBFS :: Map Point Point                -- ^ Map from Node to parent
               -> Seq.Seq Point                  -- ^ First in First Out Queue
               -> Set Point                      -- ^ Nodes which are already seen
               -> (Map Point Point, Maybe Point) -- ^ Map from Point to parent and the endpoint if one was found
        runBFS parents Seq.Empty _ = (parents, Nothing)
        runBFS parents (node :<| rest) seen | end node  = (parents, Just node)
                                            | otherwise = runBFS parents' queue' seen'
          where neighbours = filter (not . (`S.member` seen)) $ neighbourhood node
                parents'  = foldr (\k mp -> M.insert k node mp) parents neighbours
                queue'    = foldr (flip (Seq.|>)) rest neighbours
                seen'     = foldr S.insert seen neighbours

        goBack (_, Nothing) = Nothing
        goBack (parents, Just node) = Just $ go node
          where go node' = case M.lookup node' parents of
                            Nothing     -> [start]
                            Just parent -> node' : go parent
