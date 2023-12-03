module Days.Day2 (day2) where
import           Control.Monad  (void)
import           Data.Map       (Map)
import qualified Data.Map       as M
import           Finite         (dayn)
import           Lib            (Parser', int)
import           Solution       (Solution(..))
import qualified Text.Parsec    as Parsec
import           Text.Parsec    (runParser)

day2 :: Solution
day2 = Solution {day=dayn 2, partA=partA1, partB=partB1, common=parseDayTwo}

-- Parsing ---------------------------------------------------------------------
-- Game 2: 1 green, 1 blue, 1 red; 11 red, 3 blue; 1 blue, 18 red; 9 red, 1 green; 2 blue, 11 red, 1 green; 1 green, 2 blue, 10 red

data Colour
  = Red Int
  | Green Int
  | Blue Int
  deriving (Eq, Show)
data Set
  = Set
      { red   :: Int
      , green :: Int
      , blue  :: Int
      }
  deriving (Eq, Show)
instance Semigroup Set where 
  Set r g b <> Set rr gg bb = Set (max r rr) (max gg g) (max bb b)
instance Monoid Set where
  mempty = Set 0 0 0 -- is that well defined for this problem?
newtype Game
  = Game [Set]
  deriving (Eq, Show)
type GameList = Map Int Game

parseColour :: Parser' Colour
parseColour = do
  num <- int
  void Parsec.space
  colour <- Parsec.choice $ fmap Parsec.string ["red", "green", "blue"]
  pure case colour of
    "red"   -> Red num
    "green" -> Green num
    "blue"  -> Blue num
    _       -> error "This should never happen"

-- Game 2: 1 green, 1 blue, 1 red; 11 red, 3 blue; 1 blue, 18 red; 9 red, 1 green; 2 blue, 11 red, 1 green; 1 green, 2 blue, 10 red
parseGame :: Parser' (Int, Game)
parseGame = do
  void Parsec.spaces
  void $ Parsec.string "Game "
  id_ <- int
  void $ Parsec.string ": "
  -- probably need to do some changes here for part 2
  sets <- parseSet `Parsec.sepBy` Parsec.string "; "
  pure (id_, Game sets)

parseSet :: Parser' Set
parseSet = do
  colours <- parseColour `Parsec.sepBy` Parsec.string ", "
  pure $ foldr (\c acc -> case c of
    Red   i -> acc { red   = i }
    Green i -> acc { green = i }
    Blue  i -> acc { blue  = i }
    ) (Set 0 0 0) colours

parseGames :: Parser' GameList
parseGames = do
  games <- parseGame `Parsec.endBy` Parsec.newline
  pure $ M.fromList games

parseDayTwo :: String -> GameList
parseDayTwo input =   
  case runParser parseGames () "(source)" input  of
    Right a -> a
    Left  a -> error $ show a

commonDayTwo :: (Int -> Game -> Int -> Int) -> GameList -> Int
commonDayTwo hof = M.foldrWithKey hof 0

partA1 :: GameList -> Int
partA1 = commonDayTwo (\k (Game v) acc -> 
    if all (\Set {..} -> red <= redLimit && green <= greenLimit && blue <= blueLimit) v
    then k + acc
    else acc
  )
  where
  redLimit, greenLimit, blueLimit :: Int
  redLimit   = 12
  greenLimit = 13
  blueLimit  = 14

partB1 :: GameList -> Int
partB1 = commonDayTwo
  (\_ (Game v) acc -> 
    acc + pow v
  )
  where pow = (\Set {..} -> red * blue * green) . mconcat 

