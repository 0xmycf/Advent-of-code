{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Days.Day13 (day13) where
import           Control.Lens    (FoldableWithIndex (ifoldMap), both, (^..))
import           Data.List       (sort)
import           Data.List.Split (splitOn)
import           Data.Monoid     (Product (Product), Sum (..), getProduct)
import           Finite          (dayn)
import           Lib             (Parser)
import           Solution        (Solution (..))
import           Text.Parsec     (runParser)
import qualified Text.Parsec     as P

day13 :: Solution
day13 = Solution {day=dayn 13, partA=partA1, partB=partB1, common=parseDay13.splt}

data Tree a = Leaf a | Node [Tree a]
  deriving (Eq)

instance Show a => Show (Tree a) where
  show (Leaf a)  = show a
  show (Node ls) = show ls

instance Ord a => Ord (Tree a) where
   Leaf a  `compare` Leaf b  = a        `compare` b
   Node as `compare` Leaf b  = as       `compare` [Leaf b]
   Leaf a  `compare` Node bs = [Leaf a] `compare` bs
   Node as `compare` Node bs = as       `compare` bs

type Input = [(Tree Int, Tree Int)]

splt :: String -> [String]
splt = splitOn "\n\n"

parseDay13 :: [String] -> [(Tree Int, Tree Int)]
parseDay13 = fmap (right . runParser two () "")
  where right = \case
                  Left err     -> error . show $ err
                  Right parsed -> parsed

one :: Parser () (Tree Int)
one = do
      _ <- P.char '['
      num <- (leaf P.<|> one) `P.sepBy` P.char ','
      _ <- P.char ']'
      pure $ Node num

two :: Parser () (Tree Int, Tree Int)
two = do
      f <- one
      _ <- P.newline
      s <- one
      pure (f,s)

leaf :: Parser () (Tree Int)
leaf = Leaf . (read @Int) <$> P.many1 P.digit

common13 :: (FoldableWithIndex i f, Monoid a1) =>
            (i -> a2 -> a1) -> (a1 -> b) -> (t -> f a2) -> t -> b
common13 f getter modifier xs = getter $ ifoldMap f (modifier xs)

partA1, partB1 :: Input -> Int
partA1 = common13 (\i (l,r) -> if l < r then Sum $ i + 1 else Sum 0) getSum id
partB1 = common13 (\i t -> if t == dl || t == dr then Product $ i + 1 else Product 1)
                     getProduct
                    (sort . (++ divider) . (^.. traverse . both))
  where divider@[dl,dr]= case runParser two () "" "[[2]]\n[[6]]"  of
                  Left ohno   -> error . show $ ohno
                  Right (l,r) -> [l,r]
