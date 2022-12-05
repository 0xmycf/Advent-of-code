{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module Days.Day05 (day5) where
import           Control.Arrow      ((&&&))
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as Map
import           Data.List          (transpose)
import           Data.Text          (Text, pack)
import qualified Data.Text          as T
import           Finite             (Finite, finite, unwrap)
import           Solution           (Solution (..))

day5 :: Solution
day5 = Solution {day=finite 4, partA=partA1, partB=partB1, common=parseDay05}


{-
  [V]     [B]                     [C]
  [C]     [N] [G]         [W]     [P]
  [W]     [C] [Q] [S]     [C]     [M]
  [L]     [W] [B] [Z]     [F] [S] [V]
  [R]     [G] [H] [F] [P] [V] [M] [T]
  [M] [L] [R] [D] [L] [N] [P] [D] [W]
  [F] [Q] [S] [C] [G] [G] [Z] [P] [N]
  [Q] [D] [P] [L] [V] [D] [D] [C] [Z]
   1   2   3   4   5   6   7   8   9

  move 1 from 9 to 2
  move 4 from 6 to 1
-}

data Instruction = Ins
                  { _count :: Int
                  , _from  :: Finite 9
                  , _to    :: Finite 9
                  } deriving (Show)

type Instructions = [Instruction]
type Crates       = IntMap Text
type Input        = (Crates, Instructions)

parseDay05  :: String -> (Crates, Instructions)
parseDay05 = ((crates . head) &&& (instructions . last)) . T.splitOn "\n\n" . pack
  where
  crates     = Map.fromList . zip [0..]
                . fmap (T.intercalate "" . filter (not . T.null)
                . init
                . fmap (T.drop 1 . T.take 2 . T.strip))
                . transpose
                . fmap crate
                . T.lines
  crate  xs | T.null xs = []
  crate  xs = let (f,s) = T.splitAt 4 xs in f : crate s
  instructions  = fmap instruction . T.lines
  instruction   = (\[_,c,f,t] -> Ins (fromInteger c) (finite (f - 1)) (finite (t - 1)))
                . fmap ((read @Integer) . T.unpack) . T.splitOn ","
                . T.replace " to " "," . T.replace " from " "," . T.replace "move " ","

common05 :: Input -> (Text -> Text) -> Text
common05 (c, []) _ = Map.foldr (\txt acc -> T.head txt `T.cons` acc) "" c
common05 (c, instruction:xs) f = common05 (Map.union smallMap c, xs) f
  where
  keyto = fromInteger . unwrap $ _to instruction
  keyfrom = fromInteger . unwrap $ _from instruction
  gs f = c Map.! (fromInteger . unwrap $ f instruction)
  fstack = gs _from
  tbmoved = f $ T.take (_count instruction) fstack
  oldStack = T.drop (_count instruction) fstack
  updatedStack = tbmoved `T.append` gs _to
  smallMap = Map.fromList [(keyto, updatedStack), (keyfrom, oldStack)]

partA1 :: Input -> Text
partA1 input = common05 input T.reverse

partB1 :: Input -> Text
partB1 input = common05 input id
