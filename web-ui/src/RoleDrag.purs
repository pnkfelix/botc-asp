-- | JavaScript-based drag handling for role tokens
-- Uses Pointer Events for reliable cross-platform support
module RoleDrag
  ( initDragHandler
  , cleanupDragHandler
  , subscribeToDrops
  , DropEvent
  ) where

import Prelude
import Effect (Effect)

-- | Event data for when a role is dropped
type DropEvent =
  { role :: String
  , fromPlayer :: String
  , toPlayer :: String
  , time :: String
  }

-- | Initialize the drag handler (sets up pointer event listeners)
foreign import initDragHandlerImpl :: Effect Unit

initDragHandler :: Effect Unit
initDragHandler = initDragHandlerImpl

-- | Clean up the drag handler (remove event listeners)
foreign import cleanupDragHandlerImpl :: Effect Unit

cleanupDragHandler :: Effect Unit
cleanupDragHandler = cleanupDragHandlerImpl

-- | Subscribe to role drop events
-- Returns an Effect that unsubscribes when called
foreign import subscribeToDropsImpl :: (DropEvent -> Effect Unit) -> Effect (Effect Unit)

subscribeToDrops :: (DropEvent -> Effect Unit) -> Effect (Effect Unit)
subscribeToDrops = subscribeToDropsImpl
