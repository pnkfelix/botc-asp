module TextareaUtils
  ( scrollToLine
  , focusTextarea
  , scrollToText
  , scrollToTextInChild
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

-- | Scroll to and highlight specific text within a scrollable element
-- | Takes the element ID and the text to find and highlight
-- | Returns true if found and highlighted, false otherwise
foreign import scrollToTextImpl :: String -> String -> Effect Boolean

scrollToText :: String -> String -> Effect Boolean
scrollToText = scrollToTextImpl

-- | Scroll to and highlight specific text within a specific child element
-- | Takes the element ID, child index (0-indexed), and the text to find
-- | Returns true if found and highlighted, false otherwise
foreign import scrollToTextInChildImpl :: String -> Int -> String -> Effect Boolean

scrollToTextInChild :: String -> Int -> String -> Effect Boolean
scrollToTextInChild = scrollToTextInChildImpl
