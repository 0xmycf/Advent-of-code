{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
module Days.DayTwo (day2) where
import           Finite   (finite)
import           Solution (Solution (..))

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
-}

day2 :: Solution
day2 = Solution {day=finite 1, partA=flip partA2 score, partB=partB2, common=parseDayTwo}

-- | A for Rock, B for Paper, and C for Scissors
-- X for Rock, Y for Paper, and Z for Scissors
parseDayTwo  :: String -> [Round Move]
parseDayTwo input = splitted
  where
  splitted = toWho . words <$> lines input
  toWho (op:you:_) = Round (read you) (read' op)
    where
    read' "A" = Rock
    read' "B" = Paper
    read' "C" = Scissors
    read' _   = error "faulty string"
  toWho _          = error "faulty input"

partA2 :: [Round Move] -> (Round Move -> (Int, Int)) -> Int
partA2 rnds mapper = fst . foldr (\(a,b) (x,y) -> (a+x, b+y)) (0,0) $ fmap mapper rnds

partB2 :: [Round Move] -> Int
partB2 = flip partA2 mapper
  where
  mapper :: Round Move -> (Int, Int)
  mapper rnd = score $ fmap toP2 rnd
    where
    -- | X means you need to lose,
    --   Y means you need to end the round in a draw, and
    --   Z means you need to win
    toP2 :: Move -> Symbol
    toP2 m = let o = fromEnum $ _op rnd in
             case m of
              X -> toEnum $ (o + 2) `mod` 3
              Y -> _op rnd
              Z -> toEnum $ (o + 1) `mod` 3


data Round a = Round
  { _you :: a
  , _op  :: Symbol
  } deriving (Show, Functor)

data Move = X | Y | Z
  deriving (Show, Enum, Eq, Read)

data Symbol = Rock | Paper | Scissors
  deriving (Show, Enum, Eq)

mToS :: Move -> Symbol
mToS = toEnum . fromEnum

class Scorable a b | a -> b where
  score :: a -> b

instance Scorable Symbol Int where
    score = (+1) . fromEnum

instance Scorable Move Int where
  score = score . mToS

instance (Enum b, Scorable b Int) => Scorable (Round b) (Int, Int) where
  score (Round you op) = (score you + a, score op + b)
    where
    (a,b) = case (fromEnum you - fromEnum op) `mod` 3 of
              0 -> (3,3)
              1 -> (6,0)
              2 -> (0,6)
              _ -> error "unreachable"

{-
  - | R | P | S
  R | 0 | 2 | 1
  ---------------
  P | 1 | 0 | 2            => 0 == Draw
  ---------------             1 == Win  for lhs
  S | 2 | 1 | 0               2 == Loss for rhs
-}

