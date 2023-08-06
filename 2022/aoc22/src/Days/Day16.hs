{-# LANGUAGE AllowAmbiguousTypes #-}
module Days.Day16 (day16) where
import           Control.Lens    ((^.))
import           Data.Char       (chr, ord)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (fromMaybe)
import           Finite          (dayn)
import           Lib             (Point)
import qualified Lib             as L
import           Linear          (V2(..), _x, _y)
import           Solution        (Solution(..))
import           Text.Parsec     (parse)
import qualified Text.Parsec     as P

day16 :: Solution
day16 = Solution {day=dayn 16, partA=partA1, partB=partB1, common=parseDay16}

-- Parsing and datatypes ----------------------------------------------------

data Valve
  = Valve
      { coords :: Point
      , rate   :: Maybe Int
      }
  deriving (Eq, Ord)

instance Show Valve where
  show Valve{..} = "Valve(" ++ [chr (ord 'A' + coords ^. _x)] ++ "," ++ [chr (ord 'A' + coords ^. _y)] ++ ") with rate: " ++ show (fromMaybe 0 rate)

-- Valve UF has flow rate=0; tunnels lead to valves BV, AA
parseDay16 :: String -> Map Valve [Point]
parseDay16 = Map.fromList . (foo <$>) .  lines
  where foo line = case parse linep "" line of
                        Left err     -> error . show $ err
                        Right parsed -> parsed
        linep = do
                _ <- P.string "Valve "
                from' <- asPoint <$> P.many1 P.upper
                _ <- P.string " has flow rate="
                rate <- asRate <$> L.int
                _ <- P.many1 (P.noneOf ['A'..'Z'])
                to' <- (asPoint <$>) <$> P.many1 P.upper `P.sepBy` P.string ", "
                pure (Valve from' rate, to')
        asPoint (a:b:_) = V2 (ord a - ord 'A') (ord b - ord 'A')
        asPoint xs      = error $ "Parse error at: " ++ show xs
        asRate  0 = Nothing
        asRate  x = Just x

--partA1, partB1 :: Map Valve [Point] -> Int
partA1 = id
partB1 _ = Map.empty
