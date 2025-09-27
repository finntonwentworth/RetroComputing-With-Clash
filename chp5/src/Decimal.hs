
module Decimal where

import Clash.Annotations.TH
import Clash.Prelude
import RetroClash.Utils


createDomain vSystem{vName = "Dom12", vPeriod = hzToPeriod 12_000_000}

encodeDecSS :: Unsigned 4 -> Vec 7 Bool 
encodeDecSS n = unpack $ case n of 
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

topEntity 
  :: "SYS_CLK" ::: Clock Dom12
  -> "SW"      ::: Signal Dom12 (Vec 8 Bit)
  -> "SS"      ::: Signal Dom12 (SevenSegment 4 Low High High)
topEntity = withResetEnableGen board
 where
  board switches = driveSS toSegments digits 
   where
    digits = (repeat Nothing ++) <$> (map Just . bitCoerce <$> switches)
    toSegments x = (encodeDecSS x, False)


makeTopEntity 'topEntity
