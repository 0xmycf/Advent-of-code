module Days.Day14 (day14) where
import           Control.Applicative ((<|>))
import           Control.Lens
import           Control.Monad       (forM_)
import           Data.List           (groupBy, sortBy, unfoldr)
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as M
import           Data.Maybe          (fromJust)
import           Finite              (dayn)
import           Lib                 (Parser', Point, range)
import           Linear              (V2 (..))
import           Search              (bfsWithCount)
import           Solution            (Solution (..))
import qualified Text.Parsec         as P

day14 :: Solution
day14 = Solution {day=dayn 14, partA=part1, partB=part2, common=parseDay14}

data Tile = None | Sand | Bric
  deriving (Eq)

instance Show Tile where
  show None = "."
  show Sand = "o"
  show Bric = "#"

type Tetris = Map Point Tile

parseDay14 :: String -> (Int, Tetris)
parseDay14 = mkBig . mkGraph . concatMap p . lines
  where p l = case P.parse line "/input/Day14.txt" l of
                Left err     -> error $ show err
                Right parsed -> mkLine parsed
        mkGraph xs = M.fromList $ xs `zip` repeat Bric
        mkBig t = (maxy,M.union t missing)
--                                                            here is part b math hack
          where missing = M.fromList $ [V2 x y | x <- [minx - (maxy - 3)..(maxx + (maxy + 3))], y <- [0..maxy + 3]] `zip` repeat None
                maxy = maximum (M.toList t ^.. traverse . _1 . _2)
                maxx = maximum (M.toList t ^.. traverse . _1 . _1)
                minx = minimum (M.toList t ^.. traverse . _1 . _1)
        mkLine (x:y:xs) = [V2 x' y' | x' <- range (fst x) (fst y), y' <- range (snd x) (snd y)] ++ mkLine (y:xs)
        mkLine _        = []
        line :: Parser' [(Int, Int)]
        line = pair `P.sepBy` P.string " -> "
        pair :: Parser' (Int, Int)
        pair   = do
                  l <- read @Int <$> P.many1 P.digit
                  _ <- P.char ','
                  r <- read @Int <$> P.many1 P.digit
                  pure (l,r)

_showTetris :: Tetris -> IO ()
_showTetris t = forM_ t' $ \i -> do
                  putStrLn ""
                  forM_ i (putStr . show)
  where t' = t
             & M.toList
             -- flip x and y because its like that in the prompt idk
             & fmap (\(V2 k0 k1,i) -> (V2 k1 k0, i))
             & sortBy (\(V2 k0 _,_) (V2 k1 _,_) -> k0 `compare` k1)
             & groupBy (\(V2 k0 _,_) (V2 k1 _,_) -> k0 == k1)
             & fmap (fmap snd)

part1, part2 :: (Int, Tetris) -> Int
part1 (maxy, t) = length  $ unfoldr (\b -> steps maxy b <&> (b,)) t
part2 (maxy, t) = (+1) . snd . fromJust . bfsWithCount neighbourhood isEnd $ V2 500 0
  where neighbourhood p = filter (\v2 -> (v2 ^. _2) <= maxy + 2 && notElem v2 bricks) [V2 x 1 + p | x <- [1,0,-1]]
        isEnd v2 = (v2 ^. _2) > maxy + 1
        bricks = M.toList t ^.. traversed . filtered (\(_,tile) -> tile == Bric) . _1

mdown, mleft, mrigh :: Tetris -> Point -> Maybe Point
mdown mp p = M.lookup (p + V2 0 1) mp >>= \case
                                           None -> pure (p + V2 0 1)
                                           _    -> Nothing
mleft mp p = M.lookup (p + V2 (-1) 1) mp >>= \case
                                           None -> pure (p + V2 (-1) 1)
                                           _    -> Nothing
mrigh mp p = M.lookup (p + V2 1 1) mp >>= \case
                                           None -> pure (p + V2 1 1)
                                           _    -> Nothing

data MoveOutcome = NoMove Point | Move Point | LastMove

steps :: Int -> Tetris -> Maybe Tetris
steps maxy t = go (V2 500 0)
  where go :: Point -> Maybe Tetris
        go p = case step p of
                  Move   p1 -> go p1
                  NoMove p1 -> Just (t & ix p1 .~ Sand)
                  LastMove  -> Nothing

        step :: Point -> MoveOutcome
        step p = case mdown t p <|> mleft t p <|> mrigh t p of
                          Nothing | p == V2 500 0          -> LastMove      -- Its the last one (ik its hacky ignore it)
                          Nothing                          -> NoMove p      -- nothing changes anymore
                          Just p1 | (p1 ^. _2) >= maxy + 3 -> LastMove      -- If its behind the threshhold we can stop
                                  | otherwise -> Move p1

