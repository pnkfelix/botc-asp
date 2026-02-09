module Halogen.Svg.Attributes.FontStretch
  ( FontStretch(..)
  , printFontStretch
  ) where

import Prelude

data FontStretch
  = StretchNormal
  | StretchUltraCondensed
  | StretchExtraCondensed
  | StretchCondensed
  | StretchSemiCondensed
  | StretchSemiExpanded
  | StretchExpanded
  | StretchExtraExpanded
  | StretchUltraExpanded
  | StretchPercent Number

derive instance eqFontStretch :: Eq FontStretch

instance showFontStretch :: Show FontStretch where
  show = case _ of
    StretchNormal -> "StretchNormal"
    StretchUltraCondensed -> "StretchUltraCondensed"
    StretchExtraCondensed -> "StretchExtraCondensed"
    StretchCondensed -> "StretchCondensed"
    StretchSemiCondensed -> "StretchSemiCondensed"
    StretchSemiExpanded -> "StretchSemiExpanded"
    StretchExpanded -> "StretchExpanded"
    StretchExtraExpanded -> "StretchExtraExpanded"
    StretchUltraExpanded -> "StretchUltraExpanded"
    StretchPercent n -> "(StretchPercent " <> show n <> ")"

printFontStretch :: FontStretch -> String
printFontStretch = case _ of
  StretchNormal -> "normal"
  StretchUltraCondensed -> "ultra-condensed"
  StretchExtraCondensed -> "extra-condensed"
  StretchCondensed -> "condensed"
  StretchSemiCondensed -> "semi-condensed"
  StretchSemiExpanded -> "semi-expanded"
  StretchExpanded -> "expanded"
  StretchExtraExpanded -> "extra-expanded"
  StretchUltraExpanded -> "ultra-expanded"
  StretchPercent n -> show n <> "%"
