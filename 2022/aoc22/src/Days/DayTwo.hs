{-# LANGUAGE MultiParamTypeClasses #-}
module Days.DayTwo (day2) where
import           Finite   (finite)
import           Solution (Solution (..))

day2 :: Solution
day2 = Solution {day=finite 1, partA=flip partA2 score, partB=partB2, common=commonDayTwo.parseDayTwo}

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

-- | 1 for Rock, 2 for Paper, and 3 for Scissors
scoreSymbol :: Symbol -> Int
scoreSymbol Rock     = 1
scoreSymbol Paper    = 2
scoreSymbol Scissors = 3

class Scorable a b where
  score :: a -> b

data Round a = Round
  { _you :: a
  , _op  :: Symbol
  } deriving (Show, Functor)

data Move = X | Y | Z
  deriving (Show, Enum, Eq, Read)

mToS :: Move -> Symbol
mToS = toEnum . fromEnum

data Symbol = Rock | Paper | Scissors
  deriving (Show, Enum, Eq, Ord)

instance Scorable Symbol Int where
  score = scoreSymbol

instance Scorable Move Int where
  score = scoreSymbol . mToS

instance (Enum b, Scorable b Int) => Scorable (Round b) (Int, Int) where
  score (Round you op) = (score you + a, score op + b)
    where
    (a,b) = let y = fromEnum you
                o = fromEnum op
             in if | y == o               -> (3,3)
                   | (y + 1) `mod` 3 == o -> (0,6)
                   | (y + 2) `mod` 3 == o -> (6,0)
                   | otherwise            -> error "Not in bounds of Enum"


commonDayTwo :: [Round Move] -> [Round Move]
commonDayTwo = id

partA2 :: [Round Move] -> (Round Move -> (Int, Int)) -> Int
partA2 rnds mapper = fst . foldr (\(a,b) (x,y) -> (a+x, b+y)) (0,0) $ (fmap mapper rnds :: [(Int, Int)])

partB2 :: [Round Move] -> Int
partB2 = flip partA2 mapper
  where
  mapper :: Round Move -> (Int, Int)
  mapper rnd = score $ fmap toP2 rnd
    where
    --  X means you need to lose,
    --  Y means you need to end the round in a draw, and
    --  Z means you need to win
    toP2 :: Move -> Symbol
    toP2 m = let o = fromEnum $ _op rnd in
             case m of
              X -> toEnum $ (o + 2) `mod` 3
              Y -> _op rnd
              Z -> toEnum $ (o + 1) `mod` 3

