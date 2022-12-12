{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Days.Day11 (day11) where
import           Control.Arrow      ((&&&))
import           Control.Lens       (Each (each), Field1 (_1), Ixed (ix),
                                     ifiltered, makeLenses, traversed, (%@~),
                                     (%~), (&), (+~), (.~), (^..))
import           Control.Monad      (join)
import           Data.Char          (digitToInt, toLower)
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import           Data.List          (sort, unfoldr)
import           Data.List.Split    (splitOn)
import           Finite             (finite)
import           Lib                (Parser, parse)
import           Solution           (Solution (..))
import           Text.Parsec        (anyChar, char, digit, many1, manyTill,
                                     newline, sepBy, spaces, string, (<?>),
                                     (<|>))
import           Text.Read          (readMaybe)

-- Datatypes ------------------------------------------------------------------

type MonkeyIdx = Int
type Operation = Int -> Int

data Monkey = Monkey
              { _items        :: [Int]
              , _operation    :: Int -> Int
              , _test         :: Int -> Bool
              , _testNum      :: Int
              , _trueOutcome  :: MonkeyIdx
              , _falseOutcome :: MonkeyIdx
              , _activity     :: Int
              }

makeLenses ''Monkey

instance Show Monkey where
  show Monkey{..} = unwords ["Monkey", "items:", show _items, "true:",  show _trueOutcome, "false:", show _falseOutcome, "a:", show _activity  ]

monkeyF :: Parser () Monkey
monkeyF = do
          spaces
          _             <- manyTill anyChar newline <?> "first line"
          items'        <- monkeyItems
          op'           <- monkeyOperation
          (tnum, test') <- monkeyTest
          true          <- monkeyTrue
          false         <- monkeyFalse
          pure (Monkey items' op' test' tnum true false 0)

monkeyItems :: Parser () [Int]
monkeyItems = do
              spaces
              _ <- string "Starting items: "
              spaces
              fmap read <$> many1 digit `sepBy` string ", "

monkeyOperation :: Parser () Operation
monkeyOperation = do
                  spaces
                  _ <- string "Operation: new = old "
                  op' <- char '*' <|> char '+'
                  spaces
                  num <- readMaybe @Int <$> (many1 digit <|> string "old")
                  pure case num of
                    Nothing | op' == '*' -> join (*)
                            | op' == '+' -> join (+)
                    Just x  | op' == '*' -> (*x)
                            | op' == '+' -> (+x)
                    _                    -> error "faulty input"

monkeyTest :: Parser () (Int, Int -> Bool)
monkeyTest = do
              spaces
              _ <- string "Test: divisible by "
              num <- read <$> many1 digit
              pure (num, \x -> x `mod` num == 0)

monkeyBool :: Bool -> Parser () MonkeyIdx
monkeyBool bl = do
              spaces
              _ <- string $ "If " ++ (toLower <$> show bl) ++ ": throw to monkey "
              digitToInt <$> digit

monkeyTrue :: Parser () MonkeyIdx
monkeyTrue = monkeyBool True

monkeyFalse :: Parser () MonkeyIdx
monkeyFalse = monkeyBool False

-- Solving & Parsing ----------------------------------------------------------

-- | one turn for one monkey
turn :: Monkey -> Int -> IntMap Monkey -> (Int -> Int) -> IntMap Monkey
turn Monkey{..} key mp modifier = mp & ix key . activity +~ length _items -- add to the activity of the monkey
                                     & ix key . items    .~ []           --  remove all items from the monkey
                                     & (traversed.ifiltered (\i _ -> i `elem` (itms ^.. each._1)))
                                       %@~ \i mon ->
                                             let num = map snd . filter ((==i) . fst) $ itms  -- get the new numbers for the monkey
                                             in mon & items %~ (++ num)                       -- put the numbers at the back of the monkey
  where itms = _items & each %~ (modifier . _operation)                     -- first modify each item (Part one: `div` 3, Part two: Ring)
                      & each %~ ((whereTo . _test) &&& id) :: [(Int, Int)]  -- map to (number, destination)
        whereTo True  = _trueOutcome
        whereTo False = _falseOutcome

parseDay05 :: String -> [String]
parseDay05 = splitOn "\n\n"

parsecParse :: Functor f => f String -> f Monkey
parsecParse = fmap (right . parse monkeyF)
  where right = \case
                 Left  err -> error $ show err
                 Right mon -> mon

common11 :: (Int -> Int) -> Int -> [Monkey] -> Int
common11 fun idx ms = unfoldr (\b -> let ans = foldl (\acc key -> let val = acc IM.! key in turn val key acc fun) b [0..length b - 1] in Just (b,ans)) ms'
            !! idx
            ^.. traverse . activity
            & sort
            & reverse
            & take 2
            & product
  where ms'   = IM.fromList $ [0..] `zip` ms

partA1,partB1 :: [Monkey] -> Int
partA1 = common11 (`div` 3) 20

partB1 ms = common11 (`mod` limit) 10_000 ms
   where limit = product $ ms ^.. traverse.testNum

-- The Day --------------------------------------------------------------------------------
day11 :: Solution
day11 = Solution {day=finite 10, partA=partA1, partB=partB1, common=parsecParse.parseDay05}

