{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signartures #-}

module PushBtn where

import Clash.Prelude
import Clash.Annotations.TH
import RetroClash.Utils

changed
  :: (HiddenClockResetEnable dom, Eq a, NFDataX a) 
  => a -> Signal dom a -> Signal dom Bool 
changed x0 x = x ./=. register x0 x

debounce
  :: forall ps a dom. (Eq a, NFDataX a)
  => (HiddenClockResetEnable dom, KnownNat (ClockDivider dom ps)) 
  => SNat ps 
  -> a -> Signal dom a -> Signal dom a 
debounce _ start this = regEn start stable this 
  where 
    counter = register (0 :: Index (ClockDivider dom ps)) counterNext 
    counterNext = mux (changed start this) 0 (moreIdx <$> counter)
    stable = counter .== maxBound


topEntity 
  :: "CLK" ::: Clock System 
  -> "BTN" ::: Signal System (Active High) 
  -> "LED" ::: Signal System (Active High)
topEntity = withResetEnableGen board 
  where 
    board btn = toActive <$> led
      where 
        btn' = debounce (SNat @(Milliseconds 5)) False $ 
          fromActive <$> btn 
        click = isRising False btn'
        --led = regEn False click (not <$> led)
        led = oscillateWhen False click

makeTopEntity 'topEntity

