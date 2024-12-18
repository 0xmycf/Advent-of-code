{-# LANGUAGE BlockArguments, LambdaCase, OverloadedRecordDot,
             PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module Main where

import           Control.Monad       (forM_, join, unless, void, when)
import qualified Data.Char           as Char
import           Data.Foldable       (foldl')
import           Data.IORef          (newIORef, readIORef, writeIORef, modifyIORef)
import           Data.Vector.Mutable (MVector, RealWorld)
import qualified Data.Vector.Mutable as VM
import           Data.Function       (applyWhen)
import           Control.Exception   (throw, Exception)

type Id = Int
type Len = Int
data Occupation
  -- | How much of the blocks length is free
  = Free
  -- | id, len
  -- the sum of the Len should never exceed the len of the total Memory block
  | Occupied [(Id, Len)]
  deriving (Show)

sizeOfOcc :: Occupation -> Len
sizeOfOcc = \case
  Free -> undefined
  Occupied xs -> sizeOf xs

sizeOf :: [(Id, Len)] -> Len
sizeOf = foldr (\(_, len) acc -> len + acc) 0

-- >>> sizeOf [(undefined, 10), (undefined, 2)]
-- 12

-- >>> freeSizeOf (Memory {len=4, status=Free})
-- 4
--
-- >>> freeSizeOf (Memory {len=4, status=(Occupied [(1, 1), (3, 2)])})
-- 1

-- freeSizeOf :: Memory ->
freeSizeOf mem =
  case mem.status of
    Free        -> mem.len
    Occupied xs -> mem.len - sizeOf xs

data Memory
  = Memory
      { len    :: Int
        -- ^ the length of the block of memory
        {- , fileId :: Int
        -- ^ the ID of the file, basically the index
      , origId :: Int
        -- ^ keep for debugging -}
      , status :: Occupation
      }
  deriving (Show)

isFile :: Memory -> Bool
isFile mem = case mem.status of
  Free -> False
  Occupied _ -> True

input :: IO String -> IO (MVector VM.RealWorld Memory)
input in_ = do
  content <- init <$> in_
  let len = length content
  vec <- VM.new len
  forM_ ([0..len] `zip` content) $ \(i, c) ->
    VM.write vec i (Memory (Char.digitToInt c) (if even i then Occupied [(i `div` 2, Char.digitToInt c)] else Free))
  pure vec

file, test :: IO String
file = readFile "../input/day09.txt"
test = pure "2333133121414131402\n"

solution :: MVector RealWorld Memory -> IO (MVector RealWorld Memory)
solution vec = do
  front <- newIORef 0
  back <- newIORef (VM.length vec - 1)

  let loop = do {
    hd <- readIORef front;
    tl <- readIORef back;
    -- VM.mapM_ print (VM.unsafeSlice 0 10 vec);
    -- void getLine;
    -- putStrLn ("########## hd " <> show hd <> " tl " <> show tl <> " ##########");
    if hd >= tl
      then pure vec
      else do {
        free_elem <- VM.read vec hd;
        file_elem <- VM.read vec tl;
        if (isFullOrNotFree free_elem.len free_elem.status) then do
          -- putStrLn $ "hd: " <> show hd <> " and  " <> show free_elem
          -- putStrLn ("skipped " <> show hd <> " Memory: " <> show free_elem)
          writeIORef front (hd + 1)
          loop
        else if (isFree file_elem.status) then do
          -- putStrLn ("skipped " <> show tl <> " Memory: " <> show file_elem)
          -- putStrLn $ "tl: " <> show tl <> " and  " <> show free_elem
          writeIORef back (tl - 1)
          loop
        else do
          let res = swap free_elem file_elem in do
            -- putStrLn ("swap result: " <> show res)
            case res of
              Fine new bl            -> do
                -- putStrLn ("File " <> show new <> " (hd: " <> show hd <> ")")
                VM.write vec hd new
                VM.write vec tl (file_elem { status = Free })
                when bl $
                  writeIORef front (hd + 1)
                writeIORef back (tl - 1)
              Underflow moved todo -> do
                -- putStrLn $ "Underflow: " <> show moved
                VM.write vec hd moved
                VM.write vec tl todo
            loop
      }

  }

  loop

  where
    isFree = \case
      Occupied _ -> False
      Free -> True
    isFullOrNotFree :: Int -> Occupation -> Bool
    isFullOrNotFree maxSize = \case
      Free -> False
      Occupied xs -> sizeOf xs == maxSize

swap free_elem file_elem =
  case (freeSizeOf free_elem) `compare` file_elem.len of
    LT ->
      let (move, todo) = partition (freeSizeOf free_elem) file_elem
          newFull =
            case free_elem.status of
              Free -> free_elem { status = move }
              Occupied xs -> free_elem { status = cat move xs }
          newFile = file_elem { status = todo, len = sizeOfOcc todo } -- update the len too for future comparisions
      in Underflow        newFull newFile
    EQ -> Fine free_elem {status = free_elem.status >< file_elem.status } {- True, we can step -} True -- nothing but the occupation changes
    GT -> let new_elem = free_elem { status = free_elem.status >< file_elem.status } -- nothing changes, only  the implicit size is now smaller
          in if freeSizeOf new_elem < 0
          then error "Memory underflow or something"
          else Overflow new_elem

--- Memory {len = 4, status = Occupied [(8,4)]} <-- file
--- Memory {len = 3, status = Occupied [(9,2)]} <-- free
--
--- >>> partition (freeSizeOf Memory {len = 3, status = Occupied [(9,2)]}) (Memory {len = 4, status = Occupied [(8,4)]})
-- (Occupied [(8,3)],Occupied [(8,1)])

-- >>> freeSizeOf Memory {len = 3, status = Occupied [(9,2)]}
-- 1

-- >>> partition 1 (Memory {len = 4, status = Occupied [(8,4)]})
-- (Occupied [(8,3)],Occupied [(8,1)])

-- >>> swap (Memory {len = 3, status = Occupied [(9,2)]}) (Memory {len = 4, status = Occupied [(8,4)]})
-- Underflow (Memory {len = 3, status = Occupied [(9,2),(8,3)]}) (Memory {len = 1, status = Occupied [(8,1)]})

(><) :: Occupation -> Occupation -> Occupation
Occupied xs >< Occupied xs' = Occupied (xs <> xs')
Occupied xs >< Free = Occupied xs
Free >< Occupied xs' = Occupied xs'
Free >< Free = Free

cat :: Occupation ->        [(Id, Len)] -> Occupation
cat occ xs =                case occ of
  Occupied xs' -> Occupied (xs <> xs')
  Free -> Occupied xs

-- >>> partition 4 (Memory {len=5, status=(Occupied [(1, 2), (3, 1), (5,2)])})
-- (Occ        upied [(1,2),(3,1),(5,1)],Occupied [(5,1)])
--
-- >>> partition 10 (Memory {len=5, status=(Occupied [(1, 2), (3, 1), (5,2)])})
-- (Occupied [(1,2),(3,1),(5,2)],Occupied [])
--
-- >>> partition 1 (Memory {len=5, status=(Occupied [(1, 2), (3, 1), (5,2)])})
-- (Occupied [(1,1)],Occupied [(1,1),(3,1),(5,2)])
--
-- >>> partition 1 (Memory {len=5, status=(Occupied [])})
-- (Occupied [],Occupied [])
--
-- >>> partition 1 (Memory {len=5, status=(Occupied [(1,1)])})
-- (Occupied [(1,1)],Occupied [])
--
-- >>> partition 1 (Memory {len = 4, status = Occupied [(8,4)]})
-- (Occupied [(8,1)],Occupied [(8,3)])
--
-- >>> partition 1 (Memory {len = 4, status = Occupied [(8,3), (3,1)]})
-- (Occupied [(8,1)],Occupied [(8,2),(3,1)])
--
-- >>> partition 1 (Memory {len = 4, status = Occupied [(8,1)]})
-- (Occupied [(8,1)],Occupied [])

partition :: Int -> Memory -> (Occupation, Occupation)
partition i mem = case mem.status of
      Free -> error "Cannot partiton free memory"
      Occupied xs -> go [] xs 0
  where
    go acc        [] _ = (Occupied (reverse acc), Occupied [])
    -- []         8    4    []  0
    go acc (total@(id_, len):xs) size =
      if len + size > i
        -- 3 + 2 (= 5) > 4
        -- >>> abs (0 + 4 - 1)
        -- 3
        then let diff = abs (size {- + len -} - i)
              in (Occupied ( reverse ((id_, diff) : acc)), Occupied (((id_, len - diff)):xs))
        else go (total : acc) xs (len + size)

data DataSwapResult
  -- | In case we had more Free space than memory that had to be moved
  = Fine Memory Bool
  -- | Occupied Memory and the the rest of the not moved memory
  | Underflow Memory Memory
  deriving (Show)

pattern Overflow :: Memory -> DataSwapResult
pattern Overflow mem <- Fine mem bl where
  Overflow mem = Fine mem False

-- files :: MVector RealWorld Memory -> [Memory]
files :: MVector RealWorld Memory -> IO [Memory]
files = VM.foldl' (\acc mem -> applyWhen (isFile mem) (mem:) acc) []

{-
>>> vec = input file

>>> vec >>= files >>= pure .  take 3
[ Memory {len = 5, status = Occupied [(9999,5)]}
, Memory {len = 3, status = Occupied [(9998,3)]}
, Memory {len = 7, status = Occupied [(9997,7)]} ]
-}

data IllegalSwapResult = IllegalSwapResult
  deriving Show

instance Exception IllegalSwapResult

solutionB :: MVector RealWorld Memory -> IO (MVector RealWorld Memory)
solutionB vec = do
  back <- newIORef (VM.length vec - 1)
  let loop = do {
    -- VM.mapM_ print (VM.unsafeSlice 0 10 vec);
    -- void getLine;
    tl <- readIORef back;
    if tl <= 0 then
      pure vec
    else do
      -- print $ "Now at " <> show tl
      tl_elem <- VM.read vec tl;
      if (not $ isFile tl_elem) then do
        -- print $ "Skipping " <> show tl <> ", cause its not a file (" <> show tl_elem <> ")"
        modifyIORef back (subtract 1)
        loop
      else do
        hd <- findFirstMatching (sizeOfOcc tl_elem.status) tl vec
        case hd of
          Just idx -> do
            hd_elem <- VM.read vec idx
            let swap_res = swap hd_elem tl_elem
            case swap_res of
              Fine res _ -> VM.write vec idx res >> VM.write vec tl (tl_elem {status = Free})
              _          -> throw IllegalSwapResult
          Nothing  -> pure ()
        modifyIORef back (subtract 1)
        loop
  }
  loop
  where
    -- returns the first matching memory
    findFirstMatching size tl_ptr vec = do
      ref <- newIORef 0

      let go = do {
        j <- readIORef ref;
        if j >= tl_ptr then pure Nothing
        else do
          elem <- VM.read vec j
          if freeSizeOf elem >= size 
            then pure $ Just j 
            else do
              modifyIORef ref (+1)
              go
      }
      go


main :: IO ()
main =  do
  vec <- input file

  -- copying for part 2
  cpy <- VM.new (VM.length vec)
  VM.copy cpy vec

  foo <- solution vec
  bar <- solutionB cpy


  sol  <- checksum foo
  solb <- checksum bar
  -- part a
  print sol
  -- part b
  print solb

  print "hooray"
  where
    checksum vec = snd <$> VM.foldl' (\(idx, sum) val -> let (idx', size) = calculate idx val in (idx', sum + size)) (0, 0) vec

-- >>> 9 * 2 + 9 * 3 + 8 * 4
-- 77

-- >>> 9 * 2 + 9 * 3
-- 45

-- >>> calculate 2 (Memory {len=3, status=(Occupied[(9,2),(8,1)])})
-- (5,77)
--
-- >>> calculate 5 (Memory {len = 3, status = Occupied [(1,3)]})
-- (8,18)
--
-- >>> calculate 2 (Memory {len=3, status=(Occupied[(9,2)])})
-- (5,45)
--
-- >>> calculate 4 (Memory {len=3, status=(Occupied[(8,1)])})
-- (7,32)

-- | Return (newIdx, sum of prods)
calculate :: Int -> Memory -> (Int, Int)
calculate idx mem =
  let size = case mem.status of {
    Free -> 0;
    Occupied xs ->
      let (i, _) = foldl' (\(acc, offset) (id_, len) ->
                           (acc + (id_ * sum [idx+offset..offset+idx+len-1]), offset+len))
                         (0,0)
                         xs
      in i
    }
  in (idx + mem.len, size)

