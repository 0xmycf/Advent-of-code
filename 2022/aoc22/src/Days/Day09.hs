module Days.Day09 (day9) where
import           Control.Monad.State.Strict (MonadState (get, put), State,
                                             evalState, gets)
import           Data.List                  (nub)
import           Finite                     (finite)
import           Lib                        (getAllNeighbs)
import           Linear                     (V2 (V2))
import           Solution                   (Solution (..))

day9 :: Solution
day9 = Solution {day=finite 8, partA=part1, partB=part2, common=parseDay9}

parseDay9 :: String -> [Inst]
parseDay9 = fmap (read @Inst) . lines

-- | Finally used the state monad :handsup:
part1, part2 :: [Inst] -> Int
part1 is = length . nub $ ps
  where
  ps = concat $ evalState (mapM steps is) (0,0)

-- | later
part2 = const 2

-- | Does one step
step :: Inst -> State HTState (V2 Int)
step inst = do
             let dir = instToV2n inst
             (h, l) <- get
             let h' = h + dir
             if not (touching h' l)
             then
              put (h', h)
             else
              put (h', l)
             gets snd

-- | Does all the steps for one instruction
steps :: Inst -> State HTState [V2 Int]
steps inst = mapM (const (step inst)) [1..val]
  where val = instVal inst

-- | checks if the two v2's are touching
-- this could be mathyfied
touching :: V2 Int -> V2 Int -> Bool
touching w v = w == v || w `elem` getAllNeighbs v

-- Data types ------------------------------------------------------------

type HTState = (V2 Int, V2 Int)

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

