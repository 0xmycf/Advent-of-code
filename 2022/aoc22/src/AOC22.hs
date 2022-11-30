{-# OPTIONS_GHC -Wno-unused-imports #-}
module AOC22 (
  registery
  ) where
import           Finite   (finite, unwrap)
import           Solution (Solution (..))


registery :: Registry
registery = Registry
  [
  ]

-- private -----------------------------------------------------------------------------------------------------------------------

path :: FilePath
path = "./input/"

newtype Registry = Registry [Solution]

instance Show Registry where
  show (Registry rs) = foldr go "" rs
    where
    newline = "\n" :: String
    go :: Solution -> String -> String
    go val@Solution{..} acc = acc ++ "Day " ++ (show . unwrap . Solution.day $ val) ++ ": " ++ solvea ++ ", " ++ solveb ++ newline
      where
      file = path ++ show ( "day" ++ (show . (+1) . unwrap . Solution.day $ val) ++ ".input")
      parsed = common file
      solvea = partA parsed
      solveb = partB parsed

