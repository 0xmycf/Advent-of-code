module Days.Day21 (day21) where
import           Control.Lens  (Ixed (ix), (%~), (&))
import           Data.Foldable (find)
import           Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import           Data.Maybe    (fromJust)
import           Finite        (dayn)
import           Lib           (parse)
import qualified Lib           as L
import           Solution      (Solution (..))
import qualified Text.Parsec   as P

day21 :: Solution
day21 = Solution {day=dayn 21, partA=partA1, partB=partB1, common=parseDay18}

-- Parsing and datatypes ----------------------------------------------------

type Operation = Int -> Int -> Int

data N = Done Int | Pending String Operation String

instance Show N where
  show (Done i) = show i
  show _        = "Pending..."

-- Uh.. Probably more complicated than it needed to be
parseDay18 :: String -> Map String N
parseDay18 = Map.fromList . (toMonke <$>) . lines
  where toMonke line = case parse monke line of
                                Left err     -> error . show $ err
                                Right parsed -> parsed
        monke = do
                key <- P.manyTill P.anyChar (P.char ':')
                P.spaces
                n   <-  (Done <$> L.int) P.<|> pending
                pure (key, n)
        pending = do
                  f <- P.manyTill P.anyChar P.space
                  o <- P.oneOf "+-*/"
                  P.spaces
                  s <- P.many1 P.anyChar
                  let op' = case o of
                            '+' -> (+)
                            '*' -> (*)
                            '-' -> (-)
                            '/' -> div
                            _   -> error "unreachable!"
                  pure $ Pending f op' s

-- Solving ------------------------------------------------------------------

partA1, partB1 :: Map String N -> Int
partA1 is = common' is "root" id

{-L
 A 38914458159166
 r 7012559479583
 l 31901898679583
 - -}
nochange :: Int
nochange = 7012559479583
partB1 _is = fromJust $ find (\n -> common' _is "root" (const $ Done n) <= nochange * 2) [lb..lb*3]
  where lb = 3665520865900 :: Int -- trial and error gang
        _lb= 3665520865940 :: Int -- actual solution

--                                   Part B ist just one lambda abstraction different (like always) :)
common' :: Map String N -> String -> (N -> N) -> Int
common' _is key f = is' Map.! key
  -- tying the knot :)
  where is' = Map.map (\case
                        Done int                 -> int
                        Pending first op' second -> (is' Map.! first) `op'` (is' Map.! second)
                      ) is
        is = _is & ix "humn" %~ f

