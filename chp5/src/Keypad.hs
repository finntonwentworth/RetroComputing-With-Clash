{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signartures #-}

module Project where

import Clash.Annotations.TH
import Clash.Prelude
import RetroClash.Utils


createDomain vSystem{vName = "Dom12", vPeriod = hzToPeriod 12_000_000}

type Matrix rows cols a  = Vec rows (Vec cols a)
type KeyStates rows cols = Matrix rows cols Bool

scanKeypad 
  :: (KnownNat rows, KnownNat cols, IsActive rowAct, IsActive colAct)
  => (HiddenClockResetEnable dom)
  => Signal dom (Vec rows (Active rowAct))
  -> ( Signal dom (Vec cols (Active colAct))
     , Signal dom (KeyStates rows cols)
     )
scanKeypad rows = (map toActive <$> cols, transpose <$> bundle states)
  where 
    (cols, currentCol) = roundRobin nextCol
    nextCol = riseEvery (SNat @1_000)

    states = map colState indicesI

    colState thisCol = regEn (repeat False) (currentCol .== thisCol) $ 
        map fromActive <$> rows


topEntity 
  :: "CLK"      ::: Clock Dom12
  -> "SWITCHES" ::: Signal Dom12 (Vec 4 (Active Low))
  -> ("KEYPAD"  ::: Signal Dom12 (Vec 16 (Active Low))
    , "LEDS"    ::: Signal Dom12 (Vec 4 (Active Low))
    )
topEntity = withResetEnableGen board 
  where 
    board rows = (map toActive <$> leds, cols)
      where 
        (cols, keyStates) = scanKeypad rows 
        leds = bitCoerce <$> keyStates


makeTopEntity 'topEntity
