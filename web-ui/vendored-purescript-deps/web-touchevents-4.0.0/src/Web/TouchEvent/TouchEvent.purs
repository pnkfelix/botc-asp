module Web.TouchEvent.TouchEvent where

import Data.Maybe (Maybe)
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (Event)
import Web.Internal.FFI (unsafeReadProtoTagged)
import Web.TouchEvent.TouchList (TouchList)
import Web.UIEvent.UIEvent (UIEvent)

foreign import data TouchEvent :: Type

fromUIEvent :: UIEvent -> Maybe TouchEvent
fromUIEvent = unsafeReadProtoTagged "TouchEvent"

fromEvent :: Event -> Maybe TouchEvent
fromEvent = unsafeReadProtoTagged "TouchEvent"

toUIEvent :: TouchEvent -> UIEvent
toUIEvent = unsafeCoerce

toEvent :: TouchEvent -> Event
toEvent = unsafeCoerce

foreign import touches :: TouchEvent -> TouchList

foreign import targetTouches :: TouchEvent -> TouchList

foreign import changedTouches :: TouchEvent -> TouchList

foreign import altKey :: TouchEvent -> Boolean

foreign import metaKey :: TouchEvent -> Boolean

foreign import ctrlKey :: TouchEvent -> Boolean

foreign import shiftKey :: TouchEvent -> Boolean
