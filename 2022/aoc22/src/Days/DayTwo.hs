{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
module Days.DayTwo (day2) where
import           Data.Bifunctor (bimap)
import           Finite         (dayn)
import           Solution       (Solution (..))

{-
  First I implemented a naive solution that checks all cases, but Rock Paper Scissors
  is just R3 = {0, 1, 2}. Which means that you can find all solutions using (+) in R3,
  which is just simply normal (+) mod 3.

  This comes in handy, since Haskell provies @fromEnum@ and @toEnum@ functions to map between
  R3 and Move and Symbol.

  For any x, "x + 1 mod 3" x gets beaten (Rock, Paper and Paper beats Rock)
  For any x, "y = x + 2 mod 3" x beats y (Rock, Paper, Scissors and Rock beats Scissors)

  Could probably make a Semigroup instance too.

  I mindlessly used MultiParamTypeClasses although I dont know if that was at all neccessary
  but it worked nicely.

  I changed this to what I worked with in the reflectoin document
  Last git hash with old solution:

    76c78ca88ee5d91754dec4f804977de16d870db2
-}

day2 :: Solution
day2 = Solution {day=dayn 2, partA=flip partA2 scoreA, partB= partB2, common=parseDayTwo}

-- | A for Rock, B for Paper, and C for Scissors
-- X for Rock, Y for Paper, and Z for Scissors
parseDayTwo  :: String -> [Round]
parseDayTwo input = splitted
  where
  splitted = toWho . words <$> lines input
  toWho (op:you:_) = bimap fromEnum fromEnum (read' op, read @Move you)
    where
    read' "A" = Rock
    read' "B" = Paper
    read' "C" = Scissors
    read' _   = error "faulty string"
  toWho _          = error "faulty input"

partA2 :: [Round] -> (Round -> Int) -> Int
partA2 rnds mapper = sum $ fmap mapper rnds

scoreA :: Round -> Int
scoreA (b, a) = ((a - b + 1) `mod` 3) * 3 + (a + 1)

partB2 :: [Round] -> Int
partB2 = flip partA2 mapper
  where
  -- In my reflecion I wrote that
  -- ((b + o ) `mod` 3) + 1 + ((o + 1) `mod` 3) * 3 solves part b,
  -- this is not fully true: because the XYZ do not map to our DWL perfectly, we need to apply the offset -1
  mapper :: Round -> Int
  mapper (b, o) = ((b + o - 1) `mod` 3) + 1 + (o * 3)

type Round = (Int, Int)

data Move = X | Y | Z
  deriving (Show, Enum, Eq, Read)

data Shape = Rock | Paper | Scissors
  deriving (Show, Enum, Eq)

