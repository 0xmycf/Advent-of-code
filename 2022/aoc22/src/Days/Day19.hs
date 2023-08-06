{-# LANGUAGE ViewPatterns #-}
module Days.Day19 (day19) where
import           Control.Lens               (to, (%=), (&), (&~), (+=), (+~),
                                             (-=), (-~), (.~), (^.))
import           Control.Monad.State.Strict (MonadState(get), evalState)
import           Data.Coerce                (coerce)
import           Data.Default               (Default(def))
import           Data.Generics.Labels       ()
import qualified Data.HashMap.Lazy          as M
import           Data.Hashable              (Hashable)
import           Data.Int                   (Int8)
import           Data.Maybe                 (mapMaybe)
import           Finite                     (dayn)
import           GHC.Generics               (Generic)
import           Lib                        (int, parse)
import           Solution                   (Solution(..))
import           Text.Parsec                (Parsec)
import qualified Text.Parsec                as P

day19 :: Solution
day19 = Solution {day=dayn 19, partA=partA1, partB=partB1, common=common19}

-- Parsing and datatypes ----------------------------------------------------

newtype Ore
  = Ore Int8
  deriving newtype (Hashable, Num)
  deriving stock (Eq, Ord, Show)
newtype Clay
  = Clay Int8
  deriving newtype (Hashable, Num)
  deriving stock (Eq, Ord, Show)
newtype Obsidian
  = Obsidian Int8
  deriving newtype (Hashable, Num)
  deriving stock (Eq, Ord, Show)

data Blueprint
  = Bp
      { bpid     :: !Int
        -- ^ ID of the blueprint
      , ore      :: !Ore
        -- ^ each ore robot costs some ore
      , clay     :: !Ore
        -- ^ each clay robot costs some clay
      , obsidian :: !(Ore, Clay)
        -- ^ each obsidian robot costs some ore and some clay
      , geode    :: !(Ore, Obsidian)
        -- ^ each geode robot costs some ore and some obsidian
      }
  deriving (Generic, Show)

-- example input
-- Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 4 ore. Each obsidian robot costs 2 ore and 11 clay. Each geode robot costs 2 ore and 7 obsidian.
parseDay19 :: String -> [Blueprint]
parseDay19 stream = case parse blueprints stream of
                          Left err  -> error $ show err
                          Right foo -> foo

blueprints :: Parsec String () [Blueprint]
blueprints = P.many1 (blueprint >>= \bp -> P.char '\n' >> pure bp)

-- example input
-- Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 4 ore. Each obsidian robot costs 2 ore and 11 clay. Each geode robot costs 2 ore and 7 obsidian.
blueprint :: Parsec String () Blueprint
blueprint = do
  _ <- P.string "Blueprint "
  bpid <- int
  _ <- P.string ": Each ore robot costs "
  ore <- int
  _ <- P.string " ore. Each clay robot costs "
  clay <- int
  _ <- P.string " ore. Each obsidian robot costs "
  ob1 <- int
  _ <- P.string " ore and "
  ob2 <- int
  _ <- P.string " clay. Each geode robot costs "
  ge1 <- int
  _ <- P.string " ore and "
  ge2 <- int
  _ <- P.string " obsidian."
  pure Bp { bpid     = bpid
          , ore      = Ore ore
          , clay     = Ore clay
          , obsidian = (Ore ob1, Clay ob2)
          , geode    = (Ore ge1, Obsidian ge2)
          }

data GameState
  = GS
      { amntOre       :: !Ore
      , amntClay      :: !Clay
      , amntObsidian  :: !Obsidian
      , amntGeodes    :: !Int8
      , amntOreR      :: !Int8
      , amntClayR     :: !Int8
      , amntObsidianR :: !Int8
      , amntGeodeR    :: !Int8
      }
  deriving (Eq, Generic, Hashable, Ord, Show)

instance Default GameState where
  def = GS 0 0 0 0 0 0 0 0 & #amntOreR .~ (1 :: Int8)

-- Solving ------------------------------------------------------------------

type Blueprints = [Blueprint]
common19 :: String -> Blueprints
common19 = parseDay19

parts :: (Blueprint -> Int -> Int) -- ^ transformation function (before the sum/prod)
      -> Int -- ^ the limit
      -> Blueprints
      -> [Int]
parts fn limit = fmap (go limit)
  where go l bp = fn bp $ allStep l bp

partA1, partB1 :: Blueprints -> Int
partA1 = sum . parts (\bp i -> bpid bp * i) 24

partB1 (take 3 -> bps) = product $ parts (\_ i -> i) 32 bps

data PseudoMutState
  = PM
      { memo      :: M.HashMap GameState Int
      , maxResult :: !Int
      }
  deriving (Generic)

instance Default PseudoMutState where def = PM M.empty 0

allStep :: Int      -- ^ the limit (24 steps)
        -> Blueprint -- ^ the blueprint for building
        -> Int      -- ^ the maximum number of geodes
allStep limit bp = evalState (geodes (0 :: Int) (def @GameState)) (def @PseudoMutState)
  where
  geodes minute gs | minute == limit =
    let amnt = gs ^. #amntGeodes . to fromIntegral
    in do
      #memo %= M.insert gs amnt
      #maxResult %= max amnt
      pure amnt
  geodes minute gs = do
    (PM memo maxGeodes) <- get
    case memo M.!? gs of
      Just ans -> do #maxResult %= max ans; pure ans
      Nothing  -> do
        let states :: [GameState] = mapMaybe ($ maxGeodes) [buildOre, buildClay, buildObsidian, buildGeode, wait]
        m <- traverse (geodes (minute + 1)) states
        let m' = if null m
              then 0
              else maximum m
        #memo %= M.insert gs m'
        #maxResult %= max m'
        pure m'

    where
    buildOre = build (\mg -> gs ^. #amntOre >= orebp
                      && amntGeodeR < coerce orebp
                      && canBeat gs mg
                      )
                     (gs & #amntOre -~ orebp
                         & #amntOreR +~ 1)
    buildClay = build (\mg -> gs ^. #amntOre >= claybp
                       && amntClayR < coerce obsidianClaybp
                       && canBeat gs mg
                       )
                      (gs & #amntOre -~ claybp
                          & #amntClayR +~ 1)
    buildObsidian = build (\mg -> gs ^. #amntOre >= obsidianOrebp
                           && gs ^. #amntClay >= obsidianClaybp
                           && amntObsidianR < coerce geodeObsidanbp
                           && canBeat gs mg
                           )
                          ( gs &~ do
                            #amntOre -= obsidianOrebp
                            #amntClay -= obsidianClaybp
                            #amntObsidianR += 1
                           )
    buildGeode = build (\mg -> gs ^. #amntOre >= geodeOrebp
                        && gs ^. #amntObsidian >= geodeObsidanbp
                        && canBeat gs mg
                        )
                       ( gs &~ do
                          #amntOre -= geodeOrebp
                          #amntObsidian -= geodeObsidanbp
                          #amntGeodeR += 1
                        )
    wait = build (canBeat gs) gs
    build predicate s maxGeodes = if predicate maxGeodes
                        then Just $
                              s &~ do
                                #amntOre += Ore amntOreR
                                #amntClay += Clay amntClayR
                                #amntObsidian += Obsidian amntObsidianR
                                #amntGeodes += amntGeodeR
                        else Nothing
    canBeat gs' maxGeodes = gs' ^. #amntGeodes . to fromIntegral + futureGeodes gs' >= {-<=-} maxGeodes
    futureGeodes :: GameState -> Int
    futureGeodes gs' = let remaining = limit - minute
                           maxYield = remaining * gs' ^. #amntGeodeR . to fromIntegral
                                    + (remaining * (remaining - 1) `div` 2)
                        in maxYield
    amntOreR      = gs ^. #amntOreR
    amntClayR     = gs ^. #amntClayR
    amntObsidianR = gs ^. #amntObsidianR
    amntGeodeR    = gs ^. #amntGeodeR
  orebp :: Ore  = bp ^. #ore
  claybp = bp ^. #clay
  (obsidianOrebp, obsidianClaybp) = bp ^. #obsidian
  (geodeOrebp, geodeObsidanbp) :: (Ore, Obsidian) = bp ^. #geode

