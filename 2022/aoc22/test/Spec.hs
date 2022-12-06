import           Days.Day03  (day3)
import           Days.Day04  (day4)
import           Days.Day05  (day5)
import           Days.Day06  (day6)
import           Days.DayOne (day1)
import           Days.DayTwo (day2)
import           Finite      (unwrap)
import           Solution    (Solution (..))
import           Test.Hspec  (describe, hspec, it, shouldBe)

main :: IO ()
main = hspec $ do
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

solveDay :: Solution -> IO (String, String)
solveDay val@Solution{..} = do
                             s1 <- solvea
                             s2 <- solveb
                             pure (show s1, show s2)
  where
  file = readFile $ testPath ++ ( "day" ++ (show . (+1) . unwrap . Solution.day $ val) ++ ".txt")
  parsed = common <$> file
  solvea = partA  <$> parsed
  solveb = partB  <$> parsed

testPath :: FilePath
testPath = "./input/test/"
