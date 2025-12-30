-- | JavaScript-based drag handling for reminder tokens
-- Uses Pointer Events for reliable cross-platform support
module ReminderDrag
  ( initDragHandler
  , cleanupDragHandler
  , subscribeToDrops
  , DropEvent
  ) where

import Prelude
import Effect (Effect)

-- | Event data for when a reminder is dropped
type DropEvent =
  { token :: String
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

-- | Subscribe to reminder drop events
-- Returns an Effect that unsubscribes when called
foreign import subscribeToDropsImpl :: (DropEvent -> Effect Unit) -> Effect (Effect Unit)

subscribeToDrops :: (DropEvent -> Effect Unit) -> Effect (Effect Unit)
subscribeToDrops = subscribeToDropsImpl
