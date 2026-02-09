module Web.TouchEvent.EventTypes where

import Web.Event.Event (EventType(..))

touchstart :: EventType
touchstart = EventType "touchstart"

touchend :: EventType
touchend = EventType "touchend"

touchmove :: EventType
touchmove = EventType "touchmove"

touchcancel :: EventType
touchcancel = EventType "touchcancel"
