{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Days.Day07 (day7) where
import           Data.Functor    (($>))
import           Data.List       (dropWhileEnd)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Finite          (dayn)
import           Lib             (Parser)
import           Solution        (Solution (..))
import           Text.Parsec     (char, spaces, string)
import qualified Text.Parsec     as P

day7 :: Solution
day7 = Solution {day=dayn 7, partA=partA1, partB=partB1, common=common07.parseDay07}

parseDay07 :: String -> [Command]
parseDay07 input = case P.runParser (P.many1 commandParse) "" "/" input of
                        Left  err   -> error $ show err
                        Right parse -> parse

common07 :: [Command] -> Map FilePath Size
common07 = calcTotalSizes

partA1, partB1  :: Map FilePath Size -> Int
partA1 = M.foldr' (\value acc ->
                        if value > atMost
                        then acc
                        else value + acc
                          ) 0

partB1 mp = let occSize        = mp M.! "/"
                freeSize       = totalSize - occSize
                minimalReqSize = updateSize - freeSize
             in M.foldr' (\value acc ->
                          if value < acc && value >= minimalReqSize
                          then value
                          else acc
                         ) (mp M.! "/") mp

type Size = Int

data FileType = Dir FilePath
              | File FilePath !Size
  deriving (Show, Eq, Ord)

-- | Unix commands Cd and Ls
data Command = Cd FilePath              -- ^ Change Directory (.. changes up)
             | Ls [FileType] FilePath   -- ^ LiSt (lists all files in the current directory)
  deriving (Show, Eq, Ord)


-- | this is so I dont forget
atMost :: Int
atMost = 100_000

-- | this is so I dont forget
totalSize :: Int
totalSize = 70_000_000

-- | this is so I dont forget
updateSize :: Int
updateSize = 30_000_000

commandParse :: Parser FilePath Command
commandParse = do
                spaces
                _ <- char '$'
                spaces
                cmd <- string "cd" P.<|> string "ls"
                case cmd of
                  "ls" -> do
                          spaces P.<|> (P.endOfLine $> ())
                          files <- (fileP P.<|> dirP P.<?> "file or dir") `P.endBy` P.newline
                          state <- P.getState
                          return $ Ls files state
                  "cd" -> do
                          spaces
                          dir <- P.manyTill P.anyChar P.endOfLine
                          case dir of
                            "/"  -> do
                              P.setState "/"
                            ".." -> do
                              P.modifyState dotdot
                            _    -> do
                              P.modifyState (++ dir ++ "/" )
                          state <- P.getState
                          return $ Cd state
                  _ -> error "unreachable"

fileP, dirP :: Parser FilePath FileType
fileP = do
       size <- read @Int <$> P.many1 P.digit
       spaces
       name <- P.manyTill P.anyChar (P.lookAhead P.endOfLine)
       state <- P.getState
       pure $ File (state ++ name) size

dirP = do
       _ <- P.string "dir"
       spaces
       name <- P.manyTill P.anyChar (P.lookAhead P.endOfLine)
       state <- P.getState
       pure $ Dir (state ++ name ++ "/")

-- | moves one dir up duh
dotdot :: String -> String
dotdot = dropWhileEnd (/= '/') . init

calcTotalSizes :: [Command] -> Map String Int
calcTotalSizes cs = foldr addSize initMap ls
  where
  initMap = M.fromList . (`zip` repeat 0) . fmap (\(Cd dir) -> dir) . filter (\case {Cd _ -> True; _ -> False}) $ cs
  selfMap = M.fromList . zip (fmap (\(Ls _ dir) -> dir) ls) $ ls
  ls      = filter (\case {Ls _ _ -> True; _ -> False}) cs
  addSize :: Command -> Map String Int -> Map String Int
  addSize (Ls contents key) mp = let tsize = sum . map (\case
                                                        Dir dir     -> addSize (selfMap M.! dir) initMap M.! dir
                                                        File _ size -> size) $ contents
                                  in M.insertWith (+) key tsize mp

