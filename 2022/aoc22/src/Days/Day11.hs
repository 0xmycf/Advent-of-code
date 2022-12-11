module Days.Day11 (day11) where
import           Control.Lens       (At (at), Each (each), _Just, makeLenses,
                                     makePrisms, (%~), (&), (+~), (.~), (^.),
                                     (^..), (^?))
import           Data.Char          (digitToInt, toLower)
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import           Data.List          (sort, unfoldr)
import           Data.List.Split    (splitOn)
import           Data.Maybe         (fromMaybe)
import           Finite             (finite)
import           Lib                (Parser, parse)
import           Solution           (Solution (..))
import           Text.Parsec        hiding (parse)

-- Datatypes ------------------------------------------------------------------

type MonkeyIdx = Int

data Monkey = Monkey
              { _items        :: [Integer]
              , _operation    :: Operation
              , _test         :: Integer -> Bool
              , _testNum      :: Integer
              , _trueOutcome  :: MonkeyIdx
              , _falseOutcome :: MonkeyIdx
              , _activity     :: Integer
              }

data Operator = Old
              | Num Integer
              deriving (Show)

data Operation = Mult Operator
               | Plus Operator
               deriving (Show)

makeLenses ''Monkey
makePrisms ''Operator
makePrisms ''Operation

instance Show Monkey where
  -- show :: Monkey -> String
  show Monkey{..} = unwords ["Monkey", "items:", show _items, "operation:", show _operation, "true:",  show _trueOutcome, "false:", show _falseOutcome, "a:", show _activity  ]


monkeyF :: Parser () Monkey
monkeyF = do
          spaces
          _ <- manyTill anyChar newline <?> "first line"
          items' <- monkeyItems
          op'     <- monkeyOperation
          (tnum, test')  <- monkeyTest
          true   <- monkeyTrue
          false  <- monkeyFalse
          pure (Monkey items' op' test' tnum true false 0)

monkeyItems :: Parser () [Integer]
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
                  num <- read' <$> (many1 digit <|> string "old")
                  pure case op' of
                    '+' -> Plus num
                    '*' -> Mult num
                    _   -> error "faulty input"
  where read' = \case
                  "old" -> Old
                  int   -> Num $ read @Integer int

--Test: divisible by 2
monkeyTest :: Parser () (Integer, Integer -> Bool)
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
turn :: Monkey -> Int -> IntMap Monkey -> (Integer -> Integer) -> IntMap Monkey
turn monkey key mp modifier = foldl (\innermap (num, newMonkey) -> innermap & at newMonkey . _Just . items %~ (++[num])) mp' itms

  where itms = monkey ^. items & each %~ (modifier . withOp)
                               & each %~ (\x -> (x, whereTo $ monkey^.test $ x))
        op' = monkey ^. operation
        withOp i = case op' of
                    Mult b -> fromMaybe i (b ^? _Num) * i `mod` modNum
                    Plus b -> fromMaybe i (b ^? _Num) + i `mod` modNum
        whereTo b = if b then monkey ^. trueOutcome else monkey ^. falseOutcome
        mp' = let len = length $ monkey ^. items  in mp & at key . _Just . activity +~ fromIntegral len
                                                        & at key . _Just . items    .~ []
        modNum = product $ mp ^.. traverse.testNum

parseDay05 :: String -> [String]
parseDay05 = splitOn "\n\n"

parsecParse :: Functor f => f String -> f Monkey
parsecParse = fmap (right . parse monkeyF)
  where right = \case
                 Left  err -> error $ show err
                 Right mon -> mon

common11 :: (Integer -> Integer) -> Int -> [Monkey] -> Integer
common11 fun idx ms = unfoldr (\b -> let ans = foldl (\acc key -> let val = acc IM.! key in turn val key acc fun) b [0..length b - 1] in Just (b,ans)) ms' !! idx
            ^.. traverse . activity
            & sort
            & reverse
            & take 2
            & product
  where ms' = IM.fromList $ [0..] `zip` ms

partA1,partB1 :: [Monkey] -> Integer
partA1 = common11 (`div` 3) 20

partB1 = common11 id 10_000

-- The Day -------------------------------------------------------------------
day11 :: Solution
day11 = Solution {day=finite 10, partA=partA1, partB=partB1, common=parsecParse.parseDay05}

