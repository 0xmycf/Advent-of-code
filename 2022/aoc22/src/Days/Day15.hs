{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Days.Day15 (day15) where
import           Data.Range   (Bound (..), BoundType (Inclusive),
                               Range (SpanRange), mergeRanges, (+=+))
import qualified Data.Set     as Set
import           Finite       (dayn)
import           GHC.Generics (Generic)
import           Lib          (int)
import           Linear.V2    (V2 (..))
import           Solution     (Solution (..))
import           Text.Parsec  (ParseError, Parsec, eof, many1, newline, parse,
                               string)


day15 :: Solution
day15 = Solution {day=dayn 15, partA=partA1, partB=partB1, common=parseDay15}

-- Parsing and datatypes ----------------------------------------------------

parsePoint :: Parsec String () (V2 Int)
parsePoint = do
  string "x="
  x <- int
  string ", y="
  y <- int
  return $ V2 x y

manhattanDistance :: V2 Int -> V2 Int -> Int
manhattanDistance (V2 x1 y1) (V2 x2 y2) = abs (x1 - x2) + abs (y1 - y2)

-- Parser for a single line of input
parseLine :: Parsec String () (Circle, Beacon)
parseLine = do
  string "Sensor at "
  sensor <- parsePoint
  string ": closest beacon is at "
  beacon <- parsePoint <* newline
  return (Circle (manhattanDistance sensor beacon) sensor, Beacon beacon)

-- | data type for storing each line after parsing
data Circle = Circle
              { _radius :: Int
              , _center :: V2 Int
              } deriving (Show, Generic)

newtype Beacon = Beacon {unBeacon :: V2 Int}

-- | Parser for the entire input
parseInput :: String -> Either ParseError [(Circle, Beacon)]
parseInput = parse (many1 parseLine <* eof) "(unknown)"

parseDay15 :: String -> [(Circle, Beacon)]
parseDay15 input = case parseInput input of
                    Left err     -> error $ show err
                    Right parsed -> parsed

partA1, partB1 :: [(Circle, Beacon)] -> Int
-- 5 seconds...
-- partA1 csbs = Set.size $ Set.fromList [ V2 x y
--                                       | let y = ycheck
--                                       , x <- [minx - 1 .. maxx + 1]
--                                       , let v2 = V2 x y
--                                       , (not . (`Set.member` bs)) v2
--                                       , (Circle r c) <- cs
--                                       , manhattanDistance v2 c <= r
--                                       ]
partA1 csbs = subtract (Set.size bs) . len . head $ partA' ycheck cs
  -- where ycheck = 10 :: Int
  where ycheck = 2000000 :: Int
        cs     = map fst csbs
        bs     = Set.fromList $ filter (\(V2 _ y) -> y == ycheck) $ map (unBeacon . snd) csbs

-- | fast now: Thanks to accieo
partA' :: Int -> [Circle] -> [Range Int]
partA' targetY cs = mergeRanges ls where
                  ls =  [(x-w) +=+ (x+w)
                        | (Circle r (V2 x y)) <- cs
                        , let w = r - abs (targetY - y)
                        , w >= 0
                    ]

len :: Range Int -> Int
len (SpanRange (Bound x Inclusive) (Bound y Inclusive)) = y - x + 1
len _                                                   = undefined

ub :: Range Int -> Int
ub (SpanRange _ (Bound y _)) = y
ub _                         = undefined

-- slow 8 seconds
partB1 _csbs = tuning x y
  where max' = 4_000_000 :: Int
        tuning x' y' = x' * 4_000_000 + y'
        cs           = map fst _csbs
        ranges       = map (\y' -> (y', partA' y' cs)) [0..max']
        (y, (+1) . ub . head -> x)  = head $ filter (\(_,rs) -> 1 /= length rs) ranges

