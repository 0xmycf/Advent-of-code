module Solution (Solution(..)) where
import           Control.Lens (makeLenses)
import           Finite       (Finite, unwrap)

data Solution = forall a o. (Show o) => Solution
                {
                    -- | The Day indexed by 0
                    day    :: Finite 25
                ,
                    -- | the solver for part 1
                    partA  :: a -> o
                ,
                    -- | the solver for part 2
                    partB  :: a -> o
                ,
                    -- | parsing and common calculations between part A and B
                    common :: String -> a
                }

makeLenses ''Solution

instance Show Solution where
  show Solution{..} = "Solution for day " ++ show ((+1) . unwrap $ day)
