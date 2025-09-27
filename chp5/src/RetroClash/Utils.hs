{-# LANGUAGE PartialTypeSignatures, RecordWildCards, ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module RetroClash.Utils where

import Clash.Prelude
import Clash.Annotations.TH
import Data.Maybe (fromMaybe)
import qualified Data.List as L

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

-- HEX DISPLAY FUNCTIONS
-- ============================================ --

showSS :: Vec 7 Bool -> String
showSS (a :> b :> c :> d :> e :> f :> g :> Nil) = unlines . L.concat $ 
  [ L.replicate 1 $ horiz   a 
  , L.replicate 3 $ vert   f b
  , L.replicate 1 $ horiz   g 
  , L.replicate 3 $ vert   e c
  , L.replicate 1 $ horiz   d 
  ]
  where 
   horiz True  = " ##### " 
   horiz False = " ..... "

   vert b1 b2 = part b1 <> "     " <> part b2 
     where 
       part True  = "#"
       part False = "."

encodeHexSS :: Unsigned 4 -> Vec 7 Bool 
encodeHexSS n = unpack $ case n of 
  0x0 -> 0b1111110
  0x1 -> 0b0110000
  0x2 -> 0b1101101
  0x3 -> 0b1111001
  0x4 -> 0b0110011
  0x5 -> 0b1011011
  0x6 -> 0b1011111
  0x7 -> 0b1110000
  0x8 -> 0b1111111
  0x9 -> 0b1110011
  0xa -> 0b1110111
  0xb -> 0b0011111
  0xc -> 0b1001110
  0xd -> 0b0111101
  0xe -> 0b1001111
  0xf -> 0b1000111

data SevenSegment n cathodes segments dp = SevenSegment 
  { cathodes :: "CAT" ::: Vec n (Active cathodes)
  , segments :: "SEG" ::: Vec 7 (Active segments)
  , dp       :: "DP"  ::: Active dp 
  }

driveSS 
  :: (KnownNat n, HiddenClockResetEnable dom, _) 
  => (a -> (Vec 7 Bool, Bool))
  -> Signal dom (Vec n (Maybe a))
  -> Signal dom (SevenSegment n cathodes segments dp)
driveSS draw digits = do 
    cathodes <- map toActive <$> cathodes 
    segments <- map toActive <$> segments 
    dp <- toActive <$> dp
    pure SevenSegment{..}
  where 
    (cathodes, digit) = muxRR (risePeriod (SNat @(Milliseconds 1))) digits 
    (segments, dp) = unbundle $ maybe (repeat False, False) draw <$> digit

risePeriod ::
  forall ps dom.
  (HiddenClockResetEnable dom, _) =>
  SNat ps ->
  Signal dom Bool
risePeriod _ = riseEvery (SNat @(ClockDivider dom ps))

riseRate ::
  forall rate dom.
  (HiddenClockResetEnable dom, _) =>
  SNat rate ->
  Signal dom Bool
riseRate _ = risePeriod (SNat @(HzToPeriod rate))

oneHot :: forall n. (KnownNat n) => Index n -> Vec n Bool
oneHot = reverse . bitCoerce . bit @(Unsigned n) . fromIntegral

roundRobin 
  :: forall n dom a. (KnownNat n, HiddenClockResetEnable dom)
  => Signal dom Bool
  -> (Signal dom (Vec n Bool), Signal dom (Index n))
roundRobin next = (selector, i)
  where 
    i = regEn (0 :: Index n) next $ nextIdx <$> i 
    selector = bitCoerce . oneHot <$> i 

muxRR 
  :: (KnownNat n, HiddenClockResetEnable dom)
  => Signal dom Bool 
  -> Signal dom (Vec n a) 
  -> (Signal dom (Vec n Bool), Signal dom a)
muxRR tick xs = (selector, current)
  where 
    (selector, i) = roundRobin tick
    current = xs .!!. i

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

nextIdx :: (Eq a, Enum a, Bounded a) => a -> a 
nextIdx = fromMaybe minBound . succIdx

prevIdx :: (Eq a, Enum a, Bounded a) => a -> a 
prevIdx = fromMaybe maxBound . predIdx
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

type SecondPeriods dom = 1_000_000_000_000 `Div` DomainPeriod dom


data OnOff on off
  = On (Index on)
  | Off (Index off)
  deriving (Generic, NFDataX)
--
-- ============================================ -- 
-- 
(.!!.) 
  :: (KnownNat n, Enum i, Applicative f) => f (Vec n a) -> f i -> f a
(.!!.) = liftA2 (!!)

