{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signartures #-}

module Project where

import Clash.Annotations.TH
import Clash.Explicit.Prelude hiding (
  ClockDivider,
  HzToPeriod,
  Microseconds,
  Milliseconds,
  Nanoseconds,
  Picoseconds,
 )
--import Lib.RetroClash.Utils

createDomain vSystem{vName = "Dom12", vPeriod = hzToPeriod 12_000_000}

-- ============================================ --
-- These functions will be useful later
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

-- scaled the original value based around 100MHz for IceStick's 12MHz clock
-- type HzToPeriod (freq :: Nat) = (1_000_000_000_000) `Div` freq
type HzToPeriod (freq :: Nat) = (12_000_000_000) `Div` freq

type ClockDivider dom ps = ps `Div` DomainPeriod dom

type SecondPeriods dom = 1_000_000_000_000 `Div` DomainPeriod dom

type Seconds (s :: Nat) = Milliseconds (1_000 * s)
type Milliseconds (ms :: Nat) = Microseconds (1_000 * ms)
type Microseconds (us :: Nat) = Nanoseconds (1_000 * us)
type Nanoseconds (ns :: Nat) = Picoseconds (1_000 * ns)
type Picoseconds (ps :: Nat) = ps

data OnOff on off
  = On (Index on)
  | Off (Index off)
  deriving (Generic, NFDataX)

isOn :: OnOff on off -> Bool
isOn On{} = True
isOn Off{} = False

countOnOff :: (KnownNat on, KnownNat off) => OnOff on off -> OnOff on off
countOnOff (On x) = maybe (Off 0) On $ succIdx x
countOnOff (Off y) = maybe (On 0) Off $ predIdx y

blinkingSecond ::
  forall dom.
  (KnownDomain dom, _) =>
  Clock dom ->
  Reset dom ->
  Enable dom ->
  Signal dom Bit
blinkingSecond clk rst en = boolToBit . isOn <$> r
 where
  r ::
    Signal
      dom
      ( OnOff
          (ClockDivider dom (Milliseconds 500))
          (ClockDivider dom (Milliseconds 500))
      )
  r = register clk rst en (Off 0) $ countOnOff <$> r

topEntity ::
  "CLK" ::: Clock Dom12 ->
  "LED" ::: Signal Dom12 Bit
topEntity clk = blinkingSecond clk resetGen enableGen

makeTopEntity 'topEntity

-- ============================================ --
-- Other exercises
flippy ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (Bool, Bool)
flippy clk rst en = bundle (r, r')
 where
  r = register clk rst en True r'
  r' = not <$> r

slowFlippy ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System Bool
slowFlippy clk rst en = r
 where
  r = register clk rst en True (register clk rst en True (not <$> r))

-- ============================================ --
