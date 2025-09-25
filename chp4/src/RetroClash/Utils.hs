module RetroClash.Utils where

import Clash.Prelude
import Data.Maybe (fromMaybe)

data Polarity = High | Low

newtype Active (p :: Polarity) = MkActive {activeLevel :: Bit}
  deriving (Show, Eq, Ord, Generic)
active :: Bit -> Active p
active = MkActive

class IsActive p where
  fromActive :: Active p -> Bool
  toActive :: Bool -> Active p

instance IsActive High where
  fromActive = bitToBool . activeLevel
  toActive = MkActive . boolToBit

instance IsActive Low where
  fromActive = bitToBool . complement . activeLevel
  toActive = MkActive . complement . boolToBit

-- ============================================ --
-- succ and pred that respect bound
succIdx :: (Eq a, Enum a, Bounded a) => a -> Maybe a
succIdx x
  | x == maxBound = Nothing
  | otherwise = Just $ succ x

predIdx :: (Eq a, Enum a, Bounded a) => a -> Maybe a
predIdx x
  | x == minBound = Nothing
  | otherwise = Just $ pred x

moreIdx :: (Eq a, Enum a, Bounded a) => a -> a 
moreIdx = fromMaybe maxBound . succIdx

lessIdx :: (Eq a, Enum a, Bounded a) => a -> a 
lessIdx = fromMaybe maxBound . predIdx

-- ============================================ --
  
withResetEnableGen 
  :: (KnownDomain dom)
  => (HiddenClockResetEnable dom => r)
  -> Clock dom -> r 
withResetEnableGen board clk =
  withClockResetEnable clk resetGen enableGen board 

-- Oscillate boolean on external signal trigger
oscillateWhen
  :: (HiddenClockResetEnable dom)
  => Bool -> Signal dom Bool -> Signal dom Bool 
oscillateWhen init trigger = r 
  where 
    r = regEn init trigger (not <$> r)
-- ============================================ -- 
type HzToPeriod (freq :: Nat) = (12_000_000_000) `Div` freq

type SecondPeriods dom = 1_000_000_000_000 `Div` DomainPeriod dom


data OnOff on off
  = On (Index on)
  | Off (Index off)
  deriving (Generic, NFDataX)
