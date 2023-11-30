import           Criterion               (bench, bgroup, nfIO)
import qualified Criterion.Main          as CMain

main :: IO ()
main = do
    CMain.defaultMain [
      bgroup "Group 1" [ bench "1" $ nfIO ,    -- bench "example" $ whnf exampleBench 100000000
                       ]
      ]
