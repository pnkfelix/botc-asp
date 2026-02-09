module Web.TouchEvent.Touch where

import Web.Event.EventTarget (EventTarget)

foreign import data Touch :: Type

foreign import identifier :: Touch -> Int

foreign import screenX :: Touch -> Int

foreign import screenY :: Touch -> Int

foreign import clientX :: Touch -> Int

foreign import clientY :: Touch -> Int

foreign import pageX :: Touch -> Int

foreign import pageY :: Touch -> Int

foreign import target :: Touch -> EventTarget
