module Days.Day3 (day3) where
import qualified Data.Char      as Char
import           Data.List      (nub, sort)
import           Data.Map       (Map)
import qualified Data.Map       as M
import           Data.Maybe     (catMaybes, fromMaybe, mapMaybe)
import           Data.Set       (Set)
import qualified Data.Set       as S
import           Finite         (dayn)
import           Lib            (Point, getAllNeighbs, gridParser)
import           Linear         (V2(..))
import           Solution       (Solution(..))
import           Text.Read      (readMaybe)

day3 :: Solution
day3 = Solution {day=dayn 3, partA=partA1, partB=partB1, common=parseDayTwo.commonDayTwo}

-- Parsing ---------------------------------------------------------------------
-- ....*....................*.............707.352....*............/.....................801...@...............333..196........484.635......287.

type Engine = Map Point Char

parseDayTwo :: [String] -> Map Point Char
parseDayTwo = gridParser id

commonDayTwo :: String -> [String]
commonDayTwo = lines

makePoints :: Engine -> Point -> [Point]
makePoints mp = Data.List.sort . go
  where
  char = fromMaybe ' ' . (`M.lookup` mp)
  go :: Point -> [Point]
  -- jeez
  go pnt | Char.isNumber $ char pnt = goleft (pnt - V2 0 1) <> [pnt] <> goright (pnt + V2 0 1)
         | otherwise                = []
  goleft pnt | Char.isNumber $ char pnt = goleft (pnt - V2 0 1) <> [pnt]
             | otherwise                = []
  goright pnt | Char.isNumber $ char pnt = [pnt] <> goright (pnt + V2 0 1)
              | otherwise                = []

type Parts = Engine -> Int

data PartSData a
  = PAD [a] (Set [Point])
extract :: PartSData a -> [a]
extract (PAD a _) = a

partA1 :: Parts
partA1 mp = sum partNumbers
  where
  symbols = M.keys $ M.filter (\c -> not (Char.isNumber c) && c /= '.') mp
  neighbsOfNums = S.fromList $ symbols >>= getAllNeighbs
  partNumbers  =
    catMaybes . extract $ S.foldr
      (\v a@(PAD acc seen) ->
        let points = makePoints mp v in
        if S.member points seen then
          a
        else
          PAD (readMaybe (fmap (mp M.!) points) : acc) (S.insert points seen)
      ) (PAD [] S.empty) neighbsOfNums

makeNumber :: Engine -> [Point] -> Maybe Int
makeNumber mp = readMaybe . mapMaybe (`M.lookup` mp) . Data.List.sort

partB1 :: Parts
partB1 mp = sum gearNumbers
  where
  gears = M.keys . M.filter (== '*') $ mp
  neighbsOfNums = S.fromList $ Data.List.sort . getAllNeighbs <$> gears
  gearNumbers = extract $
    S.foldr (\v a@(PAD acc seen) ->
      let points = makePoints mp <$> v
          notInSet = Data.List.nub $ filter (not . null) $ filter (not . (`S.member` seen)) points
       in if length notInSet > 1 then
        PAD (product (fromMaybe 1 . makeNumber mp <$> notInSet) : acc) (foldr S.insert seen notInSet)
       else
         a
   ) (PAD [] S.empty) neighbsOfNums
