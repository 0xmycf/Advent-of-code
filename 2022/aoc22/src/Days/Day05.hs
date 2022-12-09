{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
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
  crates     = Map.fromList . zip [0..]                           -- make a map
                . fmap (T.intercalate "" . filter (not . T.null)  -- filter all empty spots out, then combine Text chunks into one text
                . init                                            -- remove the 1,2,3,4... at the bottom
                . fmap (T.drop 1 . T.take 2 . T.strip))           -- remove whitespace then take the Char from the insde of a [A] chunk
                . transpose                                       -- Transpose so that rows -> cols and we get the correct stacks
                . fmap crate                                      -- transform each line into a crate
                . T.lines                                         -- split the first chunk of the input into lines
  crate  xs | T.null xs = []                                      -- if line is emtpy return the base case []
  crate  xs = let (f,s) = T.splitAt 4 xs in f : crate s           -- if line is not empty then split after 4 chars and prepend to the recursive call of the rest string
  instructions  = fmap instruction . T.lines
  instruction   = (\[_,c,f,t] -> Ins (fromInteger c) (finite (f - 1)) (finite (t - 1))) -- Create Instructon out of each line (each line also contains "" so we ignore it at the beginning)
                . fmap ((read @Integer) . T.unpack) . T.splitOn ","                     -- split up the numbers, convert them into Integers
                . T.replace " to " "," . T.replace " from " "," . T.replace "move " "," -- remove the words, and whitespace (dont judge)

common05 :: Input -> (Text -> Text) -> Text
common05 (crates, []) _ = Map.foldr (\txt acc -> T.head txt `T.cons` acc) "" crates   -- if we dont have any instructions anymore we combine the map values into the final value
common05 (crates, instruction:rest) f = common05 (Map.union newCrates crates, rest) f -- recursive call with the updated crates and the rest of the instructions
  where
  keyto            = fromInteger . unwrap $ _to instruction                    -- The key of the stack we move to
  keyfrom          = fromInteger . unwrap $ _from instruction                  -- The key of the stack we move from
  getStack key     = crates Map.! key                                          -- helper function to get the stack for a given key
  fromStack        = getStack keyfrom                                          -- the stack from which we move
  toBeMoved        = f $ T.take (_count instruction) fromStack                 -- The stack that gets moved, f apllied to it
  droppedFromStack = T.drop (_count instruction) fromStack                     -- the new fromStack (dropped the creates that get moved)
  updatedStack     = toBeMoved `T.append` getStack keyto                       -- the updated Stack which we move to
  newCrates        = Map.fromList [(keyto, updatedStack), (keyfrom, droppedFromStack)] -- helper map to unify with old creates map

partA1 :: Input -> Text
partA1 input = common05 input T.reverse -- in part a we need to reverse the stack

partB1 :: Input -> Text
partB1 input = common05 input id -- in part b we leave the stacks as they were
