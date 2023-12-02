module Days.Day1 (day1) where
import           Control.Monad (join)
import           Data.Char     (digitToInt, isNumber)
import qualified Data.Map      as M
import           Data.Text     (Text, pack, replace, unpack)
import           Finite        (dayn)
import           Solution      (Solution(..))

day1 :: Solution
day1 = Solution {day=dayn 1, partA=partA1, partB=partB1, common=commonDayOne}

numMap :: M.Map Data.Text.Text Data.Text.Text
numMap = M.fromList $ ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"] `zip` map (Data.Text.pack . show) [1..9::Int]

commonDayOne :: String -> [String]
commonDayOne = lines

partA1 :: [String] -> Int
partA1 input = foldr (\v acc -> read [head v, last v] + acc) 0 newinput
  where
  newinput   = map filterNums input
  filterNums = filter isNumber

data Var
  = Var
      { num         :: Int
      , numbersTill :: Int
      }
  deriving (Show)
instance Eq Var where
  a == b = numbersTill a == numbersTill b
instance Ord Var where
  compare a b = numbersTill a `compare` numbersTill b

partB1 :: [String] -> Int
partB1 = foldr (\v acc -> let var =  getVar v in read (first var ++ last' var) + acc) 0
  where
  first = getThing takeWhile False
  last' = getThing takeWhileEnd True
  getVar = flip variations numMap

takeWhileEnd :: (a -> Bool) -> [a] -> [a]
takeWhileEnd f = takeWhile f . reverse

getThing :: Foldable t => ((Char -> Bool) -> [Char] -> t a) -> Bool -> [[Char]] -> String
getThing hof rev = show . num . minimum .
    map (\var ->
      let len = length . hof (not . isNumber) $ var
          revd = if rev then reverse var else var
          num = digitToInt (revd !! max 0 len)
       in Var num len
          )

variations :: String -> M.Map Data.Text.Text Data.Text.Text -> [String]
variations s = M.foldrWithKey' (\k v acc -> sreplace k v s : acc ) []
  where
  sreplace n r = Data.Text.unpack . Data.Text.replace n r . Data.Text.pack

