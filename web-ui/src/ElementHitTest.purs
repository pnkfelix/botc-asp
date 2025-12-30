-- | Element hit testing for detecting player elements under touch/mouse positions
module ElementHitTest
  ( findPlayerAtPoint
  ) where

import Prelude
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)

-- | Find the player name at a given screen position
-- Uses document.elementFromPoint to find the element, then walks up
-- the DOM tree looking for a data-player attribute
foreign import findPlayerAtPointImpl :: Number -> Number -> Effect (Nullable String)

findPlayerAtPoint :: Number -> Number -> Effect (Maybe String)
findPlayerAtPoint x y = toMaybe <$> findPlayerAtPointImpl x y
