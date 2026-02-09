module Halogen.Svg.Attributes.StrokeLineCap
  ( StrokeLineCap(..)
  , printStrokeLineCap
  ) where

import Prelude

data StrokeLineCap
  = LineCapButt
  | LineCapSquare
  | LineCapRound

derive instance eqStrokeLineCap :: Eq StrokeLineCap

instance showStrokeLineCap :: Show StrokeLineCap where
  show = case _ of
    LineCapButt -> "LineCapButt"
    LineCapSquare -> "LineCapSquare"
    LineCapRound -> "LineCapRound"

printStrokeLineCap :: StrokeLineCap -> String
printStrokeLineCap = case _ of
  LineCapButt -> "butt"
  LineCapSquare -> "square"
  LineCapRound -> "round"
