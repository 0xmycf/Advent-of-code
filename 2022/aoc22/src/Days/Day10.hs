{-# LANGUAGE ViewPatterns #-}
module Days.Day10 (day10) where
import           Control.Lens        (At (at), makeLenses, non, use, (%=), (%~),
                                      (&), (+=), (^.))
import           Control.Monad       (when)
import           Control.Monad.State (State, execState, forM_)
import           Data.IntMap.Strict  (IntMap)
import qualified Data.IntMap.Strict  as IM
import           Data.Monoid         (Sum (..))
import           Finite              (dayn)
import           Prelude             hiding (cycle)
import           Solution            (Solution (..))

-- example instructions
-- noop
-- addx 4

parseDay10 :: String -> [String]
parseDay10 = lines

-- Datatypes ---------------------------------------------------------------

type Instruction = String -- ^ Yes we gonna use a weird type alias

data CommState = CS
                 { _CStick   :: !Int         -- ^ the current tick the machine is in
                 , _CSnumber :: !Int         -- ^ the number the machine is working on
                 , _CSpartav :: Sum Int      -- ^ the product values which we need for the first part
                 , _CSimage  :: IntMap String -- ^ the thing we gotta draw
                 , _CSrow    :: !Int         -- ^ the row we draw at 0..5
                 } deriving Show

defaultState :: CommState
defaultState = CS {_CStick=0, _CSnumber=1, _CSpartav=mempty, _CSimage=IM.empty, _CSrow = 0}

makeLenses ''CommState

cycle :: State CommState ()
cycle = do
        draw
        cStick += 1
        updateMap

-- | handle a single instruction
-- after the current instruction has been handled it will handle pending values
handleInstruction :: Instruction -> State CommState ()
handleInstruction inst = case words inst of
                            ["noop"]                 -> cycle
                            ["addx", read @Int -> v] -> cycle >> cycle >> cSnumber += v
                            _                        -> error "unreachable!"

draw :: State CommState ()
draw = do
        drawPos <- use cStick
        sprite  <- use cSnumber

        when (drawPos `mod` 40 == 0 && drawPos /= 240) $ do
          cSrow += 1

        row <- use cSrow
        let td = if (drawPos `mod` 40) `elem` ([(sprite-1) .. (sprite +1)] :: [Int])
                then "#"
                else " "
        cSimage %= (\mp -> mp & at row . non "" %~ (++td))

-- | updates the map
updateMap :: State CommState ()
updateMap = do
            tick <- use cStick
            num  <- use cSnumber

            when (tick `elem` [20,60..220]) $
              cSpartav += Sum (num*tick)

common10 :: Traversable t => t Instruction -> CommState
common10 is = endState
  where ix' = mapM handleInstruction is
        endState = execState ix' defaultState

-- | down here so we can use the lenses
--part110, part210 :: [String] -> Int
part110, part210 :: CommState -> Int
part110 endState = getSum $ endState^.cSpartav

part210 = const 2


_drawIO :: CommState -> IO ()
_drawIO st = forM_ (IM.elems $ st^.cSimage) print

{- |
  Today I wanna focus on Lenses and State (and Lenses in those)
-}
day10 :: Solution
day10 = Solution {day=dayn 10, partA=part110, partB=part210, common=common10.parseDay10}
