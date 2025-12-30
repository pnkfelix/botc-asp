module TouchUtils where

import Prelude
import Web.Event.Event (Event)

-- | Foreign type for TouchEvent
foreign import data TouchEvent :: Type

-- | Get the first touch point's clientX
foreign import touchClientX :: TouchEvent -> Int

-- | Get the first touch point's clientY
foreign import touchClientY :: TouchEvent -> Int

-- | Convert TouchEvent to Event (for preventDefault)
foreign import touchToEvent :: TouchEvent -> Event
