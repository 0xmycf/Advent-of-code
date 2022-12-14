import           Finite     (unwrap)
import           Solution   (Solution (..))
import           Test.Hspec (describe, hspec, it, shouldBe)

main :: IO ()
main = hspec $ do
  -- describe "Day One" $ do
  --   it "Part A -- 24000" $ do
  --     (pa, _) <- solveDay day1
  --     pa `shouldBe` "24000"
  --   it "Part B -- 45000" $ do
  --     (_, pb) <- solveDay day1
  --     pb `shouldBe` "45000"

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
