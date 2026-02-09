module Halogen.Svg.Attributes.Transform
  ( Transform(..)
  , printTransform
  ) where

import Prelude

data Transform
  = Matrix Number Number Number Number Number Number
  | Translate Number Number
  | Scale Number Number
  | Rotate Number Number Number
  | SkewX Number
  | SkewY Number

derive instance eqTransform :: Eq Transform

instance showTransform :: Show Transform where
  show = case _ of
    Matrix a b c d e f -> "(Matrix " <> show a <> " " <> show b <> " " <> show c <> " " <> show d <> " " <> show e <> " " <> show f <> ")"
    Translate x y -> "(Translate " <> show x <> " " <> show y <> ")"
    Scale x y -> "(Scale " <> show x <> " " <> show y <> ")"
    Rotate a x y -> "(Rotate " <> show a <> " " <> show x <> " " <> show y <> ")"
    SkewX a -> "(SkewX " <> show a <> ")"
    SkewY a -> "(SkewY " <> show a <> ")"

printTransform :: Transform -> String
printTransform = case _ of
  Matrix a b c d e f -> "matrix(" <> show a <> "," <> show b <> "," <> show c <> "," <> show d <> "," <> show e <> "," <> show f <> ")"
  Translate x y -> "translate(" <> show x <> "," <> show y <> ")"
  Scale x y -> "scale(" <> show x <> "," <> show y <> ")"
  Rotate a x y -> "rotate(" <> show a <> "," <> show x <> "," <> show y <> ")"
  SkewX a -> "skewX(" <> show a <> ")"
  SkewY a -> "skewY(" <> show a <> ")"
