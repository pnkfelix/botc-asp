module Halogen.Svg.Attributes.MaskUnit
  ( MaskUnit(..)
  , printMaskUnit
  ) where

import Prelude

data MaskUnit
  = UserSpaceOnUse_
  | ObjectBoundingBox

derive instance eqMaskUnit :: Eq MaskUnit

instance showMaskUnit :: Show MaskUnit where
  show = case _ of
    UserSpaceOnUse_ -> "userSpaceOnUse"
    ObjectBoundingBox -> "objectBoundingBox"

-- This instance of Show is currently identical to printMaskUnit. That is
-- likely to change so don't rely on it

printMaskUnit :: MaskUnit -> String
printMaskUnit = case _ of
  UserSpaceOnUse_ -> "userSpaceOnUse"
  ObjectBoundingBox -> "objectBoundingBox"
