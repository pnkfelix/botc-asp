module Halogen.Svg.Attributes.Color
  ( Color(..)
  , printColor
  ) where

import Prelude

data Color
  = RGB Int Int Int
  | RGBA Int Int Int Number
  | Named String
  | NoColor

derive instance eqColor :: Eq Color

instance showColor :: Show Color where
  show = case _ of
    RGB r g b -> "(RGB " <> show r <> " " <> show g <> " " <> show b <> ")"
    RGBA r g b o -> "(RGBA " <> show r <> " " <> show g <> " " <> show b <> " " <> show o <> ")"
    Named s -> "(Named " <> show s <> ")"
    NoColor -> "NoColor"

printColor :: Color -> String
printColor = case _ of
  RGB r g b -> "rgb(" <> show r <> "," <> show g <> "," <> show b <> ")"
  RGBA r g b o -> "rgba(" <> show r <> "," <> show g <> "," <> show b <> "," <> show o <> ")"
  Named str -> str
  NoColor -> "None"
