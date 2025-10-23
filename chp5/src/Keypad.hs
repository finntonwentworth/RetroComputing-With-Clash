
module Project where

import Clash.Annotations.TH
import Clash.Prelude
import RetroClash.Utils


createDomain vSystem{vName = "Dom12", vPeriod = hzToPeriod 12_000_000}


topEntity 
  :: "ROWS"     ::: Signal System (Vec 4 Bit)
  -> "SWITCHES" ::: Signal System (Vec 4 Bit)
  -> ("LEDS"    ::: Signal System (Vec 4 Bit)
     ,"COLS"    ::: Signal System (Vec 4 Bit)
     )
topEntity rows switches = (rows, switches)


makeTopEntity 'topEntity
