{-# OPTIONS_GHC -Wno-unused-imports #-}
module AOC22 (
    registery
  , showIO
  ) where
import           Control.Applicative (Applicative (liftA2))
import           Data.Foldable       (foldrM)
import           Days.DayOne         (day1)
import           Finite              (finite, unwrap)
import           Solution            (Solution (..))


registery :: Registry
registery = Registry
  [ day1
  ]

-- private -----------------------------------------------------------------------------------------------------------------------

path :: FilePath
path = "./input/"

newtype Registry = Registry [Solution]

showIO :: Registry -> IO ()
showIO (Registry rs) = foldrM go "" rs >>= putStr
  where
  newline = "\n" :: String
  go :: Solution -> String -> IO String
  go val@Solution{..} acc = liftA2 ansStringBuilder solvea solveb
    where
    file = readFile $ path ++ ( "day" ++ (show . (+1) . unwrap . Solution.day $ val) ++ ".txt")
    parsed = common <$> file
    solvea = partA <$> parsed
    solveb = partB <$> parsed
    ansStringBuilder ansa ansb = acc ++ "Day " ++ (show . (+1) . unwrap . Solution.day $ val) ++ ": " ++ ansa ++ ", " ++ ansb ++ newline

