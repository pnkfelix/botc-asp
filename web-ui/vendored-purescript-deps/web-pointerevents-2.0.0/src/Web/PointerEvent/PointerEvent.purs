module Web.PointerEvent.PointerEvent
  ( PointerEvent
  , PointerId
  , PointerType(..)
  , altitudeAngle
  , azimuthAngle
  , fromEvent
  , fromMouseEvent
  , fromUIEvent
  , getCoalescedEvents
  , getPredictedEvents
  , height
  , isPrimary
  , pointerId
  , pointerType
  , pressure
  , tangentialPressure
  , tiltX
  , tiltY
  , toEvent
  , toMouseEvent
  , toUIEvent
  , twist
  , width
  )
  where

import Data.Maybe (Maybe)
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (Event)
import Web.Internal.FFI (unsafeReadProtoTagged)
import Web.UIEvent.UIEvent (UIEvent)
import Web.UIEvent.MouseEvent (MouseEvent)

foreign import data PointerEvent :: Type
data PointerType = Mouse | Touch | Pen
foreign import data PointerId :: Type

fromMouseEvent :: MouseEvent -> Maybe PointerEvent
fromMouseEvent = unsafeReadProtoTagged "PointerEvent"

fromUIEvent :: UIEvent -> Maybe PointerEvent
fromUIEvent = unsafeReadProtoTagged "PointerEvent"

fromEvent :: Event -> Maybe PointerEvent
fromEvent = unsafeReadProtoTagged "PointerEvent"

toMouseEvent :: PointerEvent -> MouseEvent
toMouseEvent = unsafeCoerce

toUIEvent :: PointerEvent -> UIEvent
toUIEvent = unsafeCoerce

toEvent :: PointerEvent -> Event
toEvent = unsafeCoerce

foreign import isPrimary :: PointerEvent -> Boolean
foreign import pointerId :: PointerEvent -> PointerId

foreign import pointerTypeImpl :: PointerEvent -> String

pointerType :: PointerEvent -> PointerType
pointerType e = case pointerTypeImpl e of
    "pen" -> Pen
    "touch" -> Touch
    _ -> Mouse

foreign import width :: PointerEvent -> Number
foreign import height :: PointerEvent -> Number
foreign import pressure :: PointerEvent -> Number
foreign import tangentialPressure :: PointerEvent -> Number
foreign import tiltX :: PointerEvent -> Int
foreign import tiltY :: PointerEvent -> Int
foreign import twist :: PointerEvent -> Int
foreign import altitudeAngle :: PointerEvent -> Number
foreign import azimuthAngle :: PointerEvent -> Number

foreign import getCoalescedEvents :: PointerEvent -> Array PointerEvent
foreign import getPredictedEvents :: PointerEvent -> Array PointerEvent
