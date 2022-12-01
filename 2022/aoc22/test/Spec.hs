{-# LANGUAGE QuasiQuotes #-}
import           Data.Text         (Text, unpack)
import           Days.DayOne       (day1)
import           NeatInterpolation (text)
import           Solution          (Solution (..))
import           Test.Hspec        (describe, hspec, it, shouldBe)

main :: IO ()
main = hspec $ do
  describe "Day One" $ do
    let (pa, pb) = solveDay day1 testDayOne
     in do
        it "Part A -- 24000" $ do
          pa `shouldBe` "24000"
        it "Part B -- 45000" $ do
          pb `shouldBe` "45000"

testDayOne :: Text
testDayOne =
      [text|
      1000
      2000
      3000

      4000

      5000
      6000

      7000
      8000
      9000

      10000
      |]

solveDay :: Solution -> Text -> (String, String)
solveDay Solution{..} file = (solvea, solveb)
  where
  parsed = common (unpack file)
  solvea = partA  parsed
  solveb = partB  parsed

