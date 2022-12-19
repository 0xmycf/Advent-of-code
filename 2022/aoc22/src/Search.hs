{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
module Search (bfs, bfsWithCount, bfsParents) where
import           Control.Monad   (join)
import           Data.Foldable   (foldl')
import qualified Data.Map.Strict as M
import           Data.Sequence   (Seq ((:<|)))
import qualified Data.Sequence   as Seq
import           Data.Set        (Set)

bfs :: Ord t
       => (t -> [t])            -- ^ neighbourhood
       -> (t -> Bool)           -- ^ end-point criterion
       -> t                     -- ^ starting point
       -> Maybe [t]             -- 1 returns the path if one was found
bfs neighbourhood end start = goBack $ runBFS (M.singleton start Nothing) (Seq.singleton start)
    where
  -- where runBFS :: Map Point (Maybe Point)        -- ^ Map from Node to parent
  --              -> Seq.Seq Point                  -- ^ First in First Out Queue
  --              -> (Map Point Point, Maybe Point) -- ^ Map from Point to parent and the endpoint if one was found
        runBFS parents Seq.Empty = (parents, Nothing)
        runBFS parents (node :<| rest) | end node  = (parents, Just node)
                                       | otherwise = runBFS parents' queue'
          where neighbours = filter (not . (`M.member` parents)) $ neighbourhood node
                parents'   = foldl' (\mp k -> M.insert k (Just node) mp) parents neighbours
                queue'     = foldl' (Seq.|>) rest neighbours

        goBack (_, Nothing) = Nothing
        goBack (parents, Just node) = Just $ go node
          where go node' = case join $ M.lookup node' parents of
                            Nothing     -> [start]
                            Just parent -> node' : go parent

-- | returns count of all discovered neighbours
bfsWithCount :: Ord t
       => (t -> [t])            -- ^ neighbourhood
       -> (t -> Bool)           -- ^ end-point criterion
       -> t                     -- ^ starting point
       -> Maybe ([t], Int)      -- ^ returns the path if one was found
bfsWithCount neighbourhood end start = goBack $ runBFS (M.singleton start Nothing) (Seq.singleton start) 0
    where
  -- where runBFS :: Map Point (Maybe Point)        -- ^ Map from Node to parent
  --              -> Seq.Seq Point                  -- ^ First in First Out Queue
  --              -> (Map Point Point, Maybe Point) -- ^ Map from Point to parent and the endpoint if one was found
        runBFS parents Seq.Empty c = (parents, Nothing, c)
        runBFS parents (node :<| rest) c | end node  = (parents, Just node, c)
                                         | otherwise = runBFS parents' queue' (c + length (filter (not . end) neighbours))
          where neighbours = filter (not . (`M.member` parents)) $ neighbourhood node
                parents'   = foldl' (\mp k -> M.insert k (Just node) mp) parents neighbours
                queue'     = foldl' (Seq.|>) rest neighbours

        goBack (_, Nothing,_) = Nothing
        goBack (parents, Just node, c) = Just (go node, c)
          where go node' = case join $ M.lookup node' parents of
                            Nothing     -> [start]
                            Just parent -> node' : go parent

-- | Returns the Map of parents
bfsParents :: Ord t
       => (t -> [t])            -- ^ neighbourhood
       -> (t -> Bool)           -- ^ end-point criterion
       -> t                     -- ^ starting point
       -> (Maybe [t], M.Map t (Maybe t))             -- ^ returns the path if one was found
bfsParents neighbourhood end start = goBack $ runBFS (M.singleton start Nothing) (Seq.singleton start)
    where
  -- where runBFS :: Map Point (Maybe Point)        -- ^ Map from Node to parent
  --              -> Seq.Seq Point                  -- ^ First in First Out Queue
  --              -> (Map Point Point, Maybe Point) -- ^ Map from Point to parent and the endpoint if one was found
        runBFS parents Seq.Empty = (parents, Nothing)
        runBFS parents (node :<| rest) | end node  = (parents, Just node)
                                       | otherwise = runBFS parents' queue'
          where neighbours = filter (not . (`M.member` parents)) $ neighbourhood node
                parents'   = foldl' (\mp k -> M.insert k (Just node) mp) parents neighbours
                queue'     = foldl' (Seq.|>) rest neighbours

        goBack (parents, Nothing) = (Nothing, parents)
        goBack (parents, Just node) = (Just $ go node, parents)
          where go node' = case join $ M.lookup node' parents of
                            Nothing     -> [start]
                            Just parent -> node' : go parent
