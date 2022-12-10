{-# LANGUAGE ViewPatterns #-}
module Days.Day10 (day10) where
import           Control.Lens        (At (at), makeLenses, non, use, uses, (%=),
                                      (%~), (&), (+=), (<>=), (?~), (^.))
import           Control.Monad.State (State, execState, forM_, gets)
import           Data.IntMap.Strict  (IntMap)
import qualified Data.IntMap.Strict  as IM
import           Data.Monoid         (Sum (..))
import           Finite              (finite)
import           Solution            (Solution (..))

-- example instructions
-- noop
-- addx 4

parseDay10 :: String -> [String]
parseDay10 = lines

-- Datatypes ---------------------------------------------------------------

type Instruction = String -- ^ Yes we gonna use a weird type alias

data CommState = CS
                 { _CStick    :: !Int         -- ^ the current tick the machine is in
                 , _CSnumber  :: !Int         -- ^ the number the machine is working on
                 , _CSpending :: [AddX]       -- ^ the stack of pending numbers
                 , _CSparta   :: IntMap Int   -- ^ the values which we need for the first part
                 , _CSimage   :: IntMap String -- ^ the thing we gotta draw
                 , _CSrow     :: !Int         -- ^ the row we draw at 0..5
                 } deriving Show

defaultState :: CommState
defaultState = CS {_CStick=0, _CSnumber=1, _CSpending=[], _CSparta=IM.empty, _CSimage=IM.empty, _CSrow = 0}

-- | This was made in preperation with part B
data AddX = Pending
            { _pvalue :: !Int -- ^ the actual value
            , _psince :: !Int -- ^ since which tick this is pending
            } deriving Show

makeLenses ''CommState
makeLenses ''AddX

-- | handle a single instruction
-- after the current instruction has been handled it will handle pending values
handleInstruction :: Instruction -> State CommState Int
handleInstruction inst = do
                          draw
                          cStick += 1                                               -- increase the tick by one
                          updateMap
                          case words inst of
                            ["noop"]                 -> pure ()                     -- do nothing
                            ["addx", read @Int -> v] ->
                              do
                              tick <- use cStick
                              cSpending <>= [Pending v tick]                        -- add the new pending to the pending list
                              draw
                              cStick += 1
                              updateMap
                            _                        -> error "unreachable!"
                          handlePending
                          gets _CSnumber                                             -- because we might learn something if we look at x throughout the execution

draw :: State CommState ()
draw = do
        let lit = "#" :: String
            drk = "." :: String
        drawPos <- use cStick
        sprite  <- use cSnumber
        case drawPos of
          40  -> do
            cSrow += 1
          80  -> do
            cSrow += 1
          120 -> do
            cSrow += 1
          160 -> do
            cSrow += 1
          200 -> do
            cSrow += 1
          _ -> pure ()
        row <- use cSrow
        let td = if (drawPos - (row * 40)) `elem` ([(sprite-1) .. (sprite +1)]:: [Int])
                then lit
                else drk
        cSimage %= (\mp -> mp & at row . non "" %~ (++td))

-- | updates the map
updateMap :: State CommState ()
updateMap = do
            tick <- use cStick
            num  <- use cSnumber
            case tick of
              20  -> do
                cSparta %= (\mp -> mp & at 0 ?~ (num * tick))
              60  -> do
                cSparta %= (\mp -> mp & at 1 ?~ (num * tick))
              100 -> do
                cSparta %= (\mp -> mp & at 2 ?~ (num * tick))
              140 -> do
                cSparta %= (\mp -> mp & at 3 ?~ (num * tick))
              180 -> do
                cSparta %= (\mp -> mp & at 4 ?~ (num * tick))
              220 -> do
                cSparta %= (\mp -> mp & at 5 ?~ (num * tick))
              _ -> pure ()

-- | Handle a pending number
handlePending :: State CommState ()
handlePending = do
                tick <- use cStick

                pendings <- uses cSpending (takeWhile (\addx -> addx^.psince < tick))
                cSpending %= drop (length pendings)

                forM_ pendings $ \addx ->
                  cSnumber += addx^.pvalue

common10 :: Traversable t => t Instruction -> CommState
common10 is = endState
  where ix' = mapM handleInstruction is
        endState = execState ix' defaultState

-- | down here so we can use the lenses
--part110, part210 :: [String] -> Int
part110, part210 :: CommState -> Int
part110 endState = getSum $ foldMap Sum (endState^.cSparta)

part210 = const 2


_drawIO :: CommState -> IO ()
_drawIO st = forM_ (IM.elems $ st^.cSimage) print

{- |
  Today I wanna focus on Lenses and State (and Lenses in those)
-}
day10 :: Solution
day10 = Solution {day=finite 9, partA=part110, partB=part210, common=common10.parseDay10}
