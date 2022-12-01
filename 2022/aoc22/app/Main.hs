module Main where
import           AOC22              (registry, showIO, showIOS, (!))
import           Data.Maybe         (fromMaybe)
import           System.Environment (getArgs)
import           Text.Read          (readMaybe)


main :: IO ()
main = do
  day <- getArgs
  case day of
    [x] -> putStrLn ("Solving only day " ++ x ++ ":\n") >> showOnlyOneDay x
    _   -> putStrLn "Solving all days:\n"                  >> showIO registry

showOnlyOneDay :: String -> IO ()
showOnlyOneDay day = showIOS entry >>= putStrLn
  where
  day'  = fromMaybe 1 (readMaybe day) - 1
  entry = registry ! day'

