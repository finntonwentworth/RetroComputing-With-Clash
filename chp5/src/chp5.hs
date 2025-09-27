
module Project where

import Clash.Annotations.TH
import Clash.Prelude
import RetroClash.Utils


createDomain vSystem{vName = "Dom12", vPeriod = hzToPeriod 12_000_000}


topEntity 
  :: "SYS_CLK" ::: Clock Dom12
  -> "SW"      ::: Signal Dom12 (Vec 8 Bit)
  -> "SS"      ::: Signal Dom12 (SevenSegment 4 Low High High)
topEntity = withResetEnableGen board
 where
  board switches = driveSS toSegments digits 
   where
    digits = (repeat Nothing ++) <$> (map Just . bitCoerce <$> switches)
    toSegments x = (encodeHexSS x, False)


makeTopEntity 'topEntity
