module Main where
import           AOC22              (registry, showIO, showIOS, (!))
import           Data.Maybe         (fromMaybe)
import           Finite             (finite)
import           System.Environment (getArgs)
import           Text.Read          (readMaybe)


main :: IO ()
main = do
  day <- getArgs
  case day of
    [x] -> showOnlyOneDay x
    _   -> showIO registry

showOnlyOneDay :: String -> IO ()
showOnlyOneDay day = showIOS entry >>= putStrLn
  where
  day'  = fromMaybe 0 (readMaybe day)
  entry = registry ! day'

