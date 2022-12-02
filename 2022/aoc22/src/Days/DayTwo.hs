{-# LANGUAGE MultiParamTypeClasses #-}
module Days.DayTwo (day2) where
import           Finite   (finite)
import           Solution (Solution (..))

-- TODO use group theory

day2 :: Solution
day2 = Solution {day=finite 1, partA=partA2, partB=partB2, common=commonDayTwo.parseDayTwo}

-- TODO typesafe
-- A for Rock, B for Paper, and C for Scissors
-- X for Rock, Y for Paper, and Z for Scissors
parseDayTwo  :: String -> [Round]
parseDayTwo input = splitted
  where
  splitted = toWho . words <$> lines input
  toWho (op:you:_) = Round (read' you) (read' op)
    where
    --  X means you need to lose,
    --  Y means you need to end the round in a draw, and
    --  Z means you need to win
    --  This sadly destroyed part 1
    read' "A" = Rock
    read' "X" =
                case read' op of
                Rock     -> Scissors
                Paper    -> Rock
                Scissors -> Paper
    read' "B" = Paper
    read' "Y" = read' op
    read' "C" = Scissors
    read' "Z" =
                case read' op of
                Rock     -> Paper
                Paper    -> Scissors
                Scissors -> Rock
    read' _   = error "faulty string"
  toWho _          = error "faulty input"

-- 1 for Rock, 2 for Paper, and 3 for Scissors
scoreSymbol :: Symbol -> Int
scoreSymbol Rock     = 1
scoreSymbol Paper    = 2
scoreSymbol Scissors = 3

class Scorable a b where
  score :: a -> b

data Round = Round
  { _you :: Symbol
  , _op  :: Symbol
  } deriving (Show)

data Symbol = Rock | Paper | Scissors
  deriving (Show, Enum, Eq, Ord)

instance Scorable Symbol Int where
  score = scoreSymbol

instance Scorable Round (Int, Int) where
  score (Round you op) = (score you + a, score op + b)
    where
    (a,b) = case (you, op) of
              (Rock, Paper)     -> (0, 6)
              (Rock, Scissors)  -> (6, 0)
              (Paper, Rock)     -> (6, 0)
              (Paper, Scissors) -> (0, 6)
              (Scissors, Rock)  -> (0, 6)
              (Scissors, Paper) -> (6, 0)
              _                 -> (3,3)

commonDayTwo :: [Round] -> [Round]
commonDayTwo = id

partA2 :: [Round] -> Int
partA2 rnd = fst . foldr (\(a,b) (x,y) -> (a+x, b+y)) (0,0) $ (fmap score rnd :: [(Int, Int)])

partB2 :: [Round] -> Int
partB2 = partA2
