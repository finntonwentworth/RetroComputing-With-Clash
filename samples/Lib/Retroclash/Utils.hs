module Lib.RetroClash.Utils (
  Polarity,
  Active,
  IsActive,
  succIdx,
  predIdx,
)
where

import Clash.Prelude

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

-- ============================================ --
