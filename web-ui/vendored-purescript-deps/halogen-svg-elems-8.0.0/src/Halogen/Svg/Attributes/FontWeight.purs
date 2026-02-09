module Halogen.Svg.Attributes.FontWeight
  ( FontWeight(..)
  , printFontWeight
  ) where

import Prelude

data FontWeight
  = FWeightNormal
  | FWeightBold
  | FWeightBolder
  | FWeightNumber Number

derive instance eqFontWeight :: Eq FontWeight

instance showFontWeight :: Show FontWeight where
  show = case _ of
    FWeightNormal -> "FWeightNormal"
    FWeightBold -> "FWeightBold"
    FWeightBolder -> "FWeightBolder"
    FWeightNumber n -> "(FWeightNumber " <> show n <> ")"

printFontWeight :: FontWeight -> String
printFontWeight = case _ of
  FWeightNormal -> "normal"
  FWeightBold -> "bold"
  FWeightBolder -> "bolder"
  FWeightNumber n -> show n
