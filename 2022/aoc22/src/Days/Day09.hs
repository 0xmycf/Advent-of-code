module Days.Day09 (day9) where
import           Control.Monad.State.Strict (MonadState (get, put), State,
                                             evalState, gets)
import           Data.List                  (nub)
import           Finite                     (dayn)
import           Lib                        (Point, getAllNeighbs)
import           Linear                     (V2 (V2))
import           Solution                   (Solution (..))

day9 :: Solution
day9 = Solution {day=dayn 9, partA=part1, partB=part2, common=parseDay9}

parseDay9 :: String -> [Inst]
parseDay9 = fmap (read @Inst) . lines

common09 :: [V2 Int] -> [Inst] -> Int
common09 v2s is = length . nub $ ps
  where
  ps = concat $ evalState (mapM steps is) (0, v2s)

-- | Finally used the state monad :handsup:
part1, part2 :: [Inst] -> Int
part1 = common09 [0]
part2 = common09 (replicate 9 0)

-- | Does one step
step :: Inst -> State HTState (V2 Int)
step inst = do
             let dir = instToV2n inst
             (h, ls) <- get
             let h'  = h + dir
             let ls' = evalState (mapM follow ls) h'

             put (h', ls')

             gets (last . snd)

-- | Follows one point
follow :: Point              -- ^ the point that we check
        -> State Point Point -- ^ head -> new pos of v2 that we check
follow v2 = do
              h <- get
              let dv = signum $ h - v2 -- Sadly, I stole that :(
              if h `touches` v2
              then
                put v2        -- if they touch nothing gets changed
              else
                put (v2 + dv) -- as shown this works
              get             -- we return the new vector in all cases

-- | Does all the steps for one instruction
steps :: Inst -> State HTState [V2 Int]
steps inst = mapM (const (step inst)) [1..val]
  where val = instVal inst

-- | checks if the two v2's are touching
-- this could be mathyfied
touches :: V2 Int -> V2 Int -> Bool
touches w v = w == v || w `elem` getAllNeighbs v

-- Data types ------------------------------------------------------------

type HTState = (V2 Int, [V2 Int])
--type HTState' =  --   Head and tail

data Inst = R Int -- ^ Right by amnt
          | L Int -- ^ Left  by amnt
          | U Int -- ^ Up    by amnt
          | D Int -- ^ Down  by amnt
  deriving (Show, Read)

-- | Maps the instruction to a vector in the same direction
instToV2n :: Inst -> V2 Int
instToV2n (R _) = V2   1   0
instToV2n (L _) = V2 (-1)  0
instToV2n (U _) = V2   0   1
instToV2n (D _) = V2   0 (-1)

-- | gets the inner value from the @Inst@
instVal :: Inst -> Int
instVal (R i) = i
instVal (L i) = i
instVal (U i) = i
instVal (D i) = i

