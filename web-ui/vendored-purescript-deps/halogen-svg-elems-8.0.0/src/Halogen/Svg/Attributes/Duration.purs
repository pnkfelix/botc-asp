module Halogen.Svg.Attributes.Duration where

import Data.Maybe (Maybe(..))

-- | Use `defaultDuration` where all values are set to `Nothing`
-- | and override the values you need.
-- | See https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/dur
type Duration =
  { hours :: Maybe Number
  , minutes :: Maybe Number
  , seconds :: Maybe Number
  , milliseconds :: Maybe Number
  }

defaultDuration :: Duration
defaultDuration =
  { hours: Nothing
  , minutes: Nothing
  , seconds: Nothing
  , milliseconds: Nothing
  }
