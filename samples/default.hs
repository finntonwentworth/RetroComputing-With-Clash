module Project where

import Clash.Annotations.TH
import Clash.Prelude

topEntity ::
  "BTN" ::: Signal System Bool ->
  "LED" ::: Signal System Bool
topEntity = id

makeTopEntity 'topEntity
