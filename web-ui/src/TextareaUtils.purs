module TextareaUtils
  ( scrollToLine
  , focusTextarea
  ) where

import Prelude

import Effect (Effect)

-- | Scroll a textarea to a specific line and select/highlight it
-- | Takes the element ID and line number (1-indexed)
foreign import scrollToLineImpl :: String -> Int -> Effect Unit

scrollToLine :: String -> Int -> Effect Unit
scrollToLine = scrollToLineImpl

-- | Focus a textarea by its element ID
foreign import focusTextareaImpl :: String -> Effect Unit

focusTextarea :: String -> Effect Unit
focusTextarea = focusTextareaImpl
