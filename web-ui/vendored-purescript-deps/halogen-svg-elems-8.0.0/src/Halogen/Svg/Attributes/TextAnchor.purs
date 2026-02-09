module Halogen.Svg.Attributes.TextAnchor
  ( TextAnchor(..)
  , printTextAnchor
  ) where

import Prelude

data TextAnchor
  = AnchorStart
  | AnchorMiddle
  | AnchorEnd

derive instance eqTextAnchor :: Eq TextAnchor

instance showTextAnchor :: Show TextAnchor where
  show = case _ of
    AnchorStart -> "Start"
    AnchorMiddle -> "AnchorMiddle"
    AnchorEnd -> "End"

printTextAnchor :: TextAnchor -> String
printTextAnchor = case _ of
  AnchorStart -> "start"
  AnchorMiddle -> "middle"
  AnchorEnd -> "end"
