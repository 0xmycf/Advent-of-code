module Days.Day25 (day25) where
import           Data.Foldable (Foldable (foldl'))
import           Finite        (dayn)
import           Solution      (Solution (..))

-- The Day ------------------------------------------------------------------
day25 :: Solution
day25 = Solution {day=dayn 25, partA=partA1, partB=partB1, common=parseDay18}

-- Parsing and Datatypes ----------------------------------------------------

newtype SNAFU = Snafu Int
  deriving newtype (Num)

instance Show SNAFU where
  show (Snafu i) = decimalToSnafu i

decimalToSnafu :: Int -> String
decimalToSnafu = go ""
  where go :: String -> Int -> String
        go xs 0 = xs >>= toSnafuChar
        go xs i = let d = i `div` 5
                      r = i `mod` 5
                      o | r == 3 || r == 4 = 1 -- we need to add one, eg at 8: 10 - 2 -> 8 not 5 + 3 like normal 5
                        | otherwise        = 0
                   in go (show r ++ xs) (d + o)
        toSnafuChar '3' = "="
        toSnafuChar '4' = "-"
        toSnafuChar x   = [x]

-- 10=-=110-0-2-
parseDay18 :: String -> [SNAFU]
parseDay18 = (toSNAFU <$>) . lines
  where toSNAFU :: String -> SNAFU
        toSNAFU = Snafu . snafuToDecimal

snafuToDecimal :: [Char] -> Int
snafuToDecimal = go (0::Int) . reverse
  where go :: (Integral t, Num a) => t -> [Char] -> a
        go _ []     = 0
        go i (x:xs) = (5^i) * charToSnafu x + go (succ i) xs
        charToSnafu '0' = 0
        charToSnafu '1' = 1
        charToSnafu '2' = 2
        charToSnafu '-' = -1
        charToSnafu '=' = -2
        charToSnafu _   = undefined -- if this happens the input is broken

-- Solutions ----------------------------------------------------------------
-- snfs - SNAFU's
partA1, partB1 :: [SNAFU] -> SNAFU
partA1 = foldl' (+) 0

partB1 _ = 2
