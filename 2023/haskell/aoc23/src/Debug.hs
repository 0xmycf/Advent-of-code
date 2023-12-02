module Debug where

import           Control.Monad (join)
import qualified Debug.Trace

js :: Show a => a -> a
js = join Debug.Trace.traceShow

