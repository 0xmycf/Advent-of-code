{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
import           Days.Day03      (day3)
import           Days.Day04      (day4)
import           Days.Day05      (day5)
import           Days.Day06      (day6)
import           Days.Day08      (day8)
import           Days.Day09      (day9)
import           Days.Day10      (day10)
import           Days.Day11      (day11)
import           Days.Day12      (day12)
import           Days.Day15      (day15)
import           Days.DayOne     (day1)
import           Days.DayTwo     (day2)
import           Finite          (unwrap)
import           MinMax          (MinMax (..))
import           Solution        (Solution (..))
import           Test.Hspec      (describe, hspec, it, shouldBe)
import           Test.QuickCheck (Testable (property), (===))


main :: IO ()
main = hspec $ do

  describe "MinMax tests" $ do
    describe "Semigroup" $ do
      it "Associativity" $ do
        property $ \x x1 x2 ->
                    MinMax x <> (MinMax x1 <> MinMax x2) ===
                    (MinMax x <> MinMax x1) <> MinMax x2

  describe "Day One" $ do
    it "Part A -- 24000" $ do
      (pa, _) <- solveDay day1
      pa `shouldBe` "24000"
    it "Part B -- 45000" $ do
      (_, pb) <- solveDay day1
      pb `shouldBe` "45000"

  describe "Day Two" $ do
    it "Part A -- 15" $ do
      (pa, _) <- solveDay day2
      pa `shouldBe` "15"
    it "Part B -- 12" $ do
      (_, pb) <- solveDay day2
      pb `shouldBe` "12"

  describe "Day 3" $ do
    it "Part A -- 157" $ do
      (pa, _) <- solveDay day3
      pa `shouldBe` "157"
    it "Part B -- 70" $ do
      (_, pb) <- solveDay day3
      pb `shouldBe` "70"

  describe "Day 4" $ do
    it "Part A -- 2" $ do
      (pa, _) <- solveDay day4
      pa `shouldBe` "2"
    it "Part B -- 4" $ do
      (_, pb) <- solveDay day4
      pb `shouldBe` "4"

  describe "Day 5" $ do
    it "Part A -- CMZ" $ do
      (pa, _) <- solveDay day5
      pa `shouldBe` show ("CMZ" :: String)
    it "Part B -- MCD" $ do
      (_, pb) <- solveDay day5
      pb `shouldBe` show ("MCD" :: String )

  describe "Day 6" $ do
    it "Part A -- 10" $ do
      (pa, _) <- solveDay day6
      pa `shouldBe` "10"
    it "Part B -- 29" $ do
      (_, pb) <- solveDay day6
      pb `shouldBe` "29"

  describe "Day 8" $ do
    it "Part A -- 21" $ do
      (pa, _) <- solveDay day8
      pa `shouldBe` "21"
    it "Part B -- 8" $ do
      (_, pb) <- solveDay day8
      pb `shouldBe` "8"

  describe "Day 9" $ do
    it "Part A -- 13" $ do
      (pa, _) <- solveDay day9
      pa `shouldBe` "13"
    it "Part B -- 36" $ do
      (_, pb) <- solveDayWith "./input/test/b" day9
      pb `shouldBe` "36"

  describe "Day 10" $ do
    it "Part A -- 13140" $ do
      (pa, _) <- solveDay day10
      pa `shouldBe` "13140"

  describe "Day 11" $ do
    it "Part A -- 10605" $ do
      (pa, _) <- solveDay day11
      pa `shouldBe` "10605"
    it "Part B -- 2713310158" $ do
      (_, pb) <- solveDay day11
      pb `shouldBe` "2713310158"

  describe "Day 12" $ do
    it "Part A -- 31" $ do
      (pa, _) <- solveDay day12
      pa `shouldBe` "31"
    it "Part B -- 29" $ do
      (_, pb) <- solveDay day12
      pb `shouldBe` "29"

  describe "Day 15" $ do
    it "Part A -- 26" $ do
      (pa, _) <- solveDay day15
      pa `shouldBe` "26"
    it "Part B -- 56000011" $ do
      (_, pb) <- solveDay day15
      pb `shouldBe` "56000011"

solveDay :: Solution -> IO (String, String)
solveDay = solveDayWith testPath

solveDayWith :: FilePath -> Solution -> IO (String, String)
solveDayWith fp val@Solution{..} = do
                             s1 <- solvea
                             s2 <- solveb
                             pure (show s1, show s2)
  where
  file = readFile $ fp ++ ( "day" ++ (show . (+1) . unwrap . Solution.day $ val) ++ ".txt")
  parsed = common <$> file
  solvea = partA  <$> parsed
  solveb = partB  <$> parsed

testPath :: FilePath
testPath = "./input/test/"
