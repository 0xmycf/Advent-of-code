module Debug where

import           Control.Monad (join)
import qualified Debug.Trace

js :: Show a => a -> a
js = join Debug.Trace.traceShow

ghciSol :: Show b => FilePath -> (String -> b) -> IO ()
ghciSol fp hof = readFile fp >>= print . hof
