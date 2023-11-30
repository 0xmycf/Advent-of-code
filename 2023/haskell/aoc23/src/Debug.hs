module Debug where

import           Control.Monad (join)
import qualified Debug.Trace

traceSI :: Show a => a -> a
traceSI = join Debug.Trace.traceShow

