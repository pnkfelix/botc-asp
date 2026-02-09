module Web.PointerEvent.Element
  ( hasPointerCapture
  , releasePointerCapture
  , setPointerCapture
  )
  where
import Prelude
import Effect (Effect)
import Web.PointerEvent.PointerEvent (PointerId)
import Web.DOM.Element (Element)

foreign import setPointerCapture :: PointerId -> Element -> Effect Unit
foreign import releasePointerCapture :: PointerId -> Element -> Effect Unit
foreign import hasPointerCapture :: PointerId -> Element -> Effect Boolean