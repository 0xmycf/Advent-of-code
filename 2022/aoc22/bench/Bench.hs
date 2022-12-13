import           AOC22                    (len, registry, solveSolution, (!))
import           Control.Concurrent.Async (mapConcurrently)
import           Criterion                (bench, bgroup, env, nf, nfIO, whnf)
import qualified Criterion.Main           as CMain

path :: FilePath
path = "./input/"

getFiles :: IO [String]
getFiles = mapConcurrently readFile [file | x <- [1..len registry], let file = path ++ "day" ++ show x ++ ".txt" ]

main :: IO ()
main = do
    CMain.defaultMain [
      env getFiles $ \ ~files ->
        bgroup "main" [
          bgroup "Day 1"  [ bench "A" $ nf (solveSolution (head files)) (registry ! 0)
                          ],
          bgroup "Day 2"  [ bench "A" $ nf (solveSolution (files Prelude.!! 1)) (registry ! 1)
                          ],
          bgroup "Day 3"  [ bench "A" $ nf (solveSolution (files Prelude.!! 2)) (registry ! 2)
                          ],
          bgroup "Day 4"  [ bench "A" $ nf (solveSolution (files Prelude.!! 3)) (registry ! 3)
                          ],
          bgroup "Day 5"  [ bench "A" $ nf (solveSolution (files Prelude.!! 4)) (registry ! 4)
                          ],
          bgroup "Day 6"  [ bench "A" $ nf (solveSolution (files Prelude.!! 5)) (registry ! 5)
                          ],
          bgroup "Day 7"  [ bench "A" $ nf (solveSolution (files Prelude.!! 6)) (registry ! 6)
                          ],
          bgroup "Day 8"  [ bench "A" $ nf (solveSolution (files Prelude.!! 7)) (registry ! 7)
                          ],
          bgroup "Day 9"  [ bench "A" $ nf (solveSolution (files Prelude.!! 8)) (registry ! 8)
                          ],
          bgroup "Day 10" [ bench "A" $ nf (solveSolution (files Prelude.!! 9)) (registry ! 9)
                          ],
          bgroup "Day 11" [ bench "A" $ nf (solveSolution (files Prelude.!! 10)) (registry ! 10)
                          ],
          bgroup "Day 12" [ bench "A" $ nf (solveSolution (files Prelude.!! 11)) (registry ! 11)
                          ],
          bgroup "Day 13" [ bench "A" $ nf (solveSolution (files Prelude.!! 12)) (registry ! 12)
                          ]
        ]
      ]
