module Halogen.Svg.Attributes.StrokeLineJoin
  ( StrokeLineJoin(..)
  , printStrokeLineJoin
  ) where

import Prelude

data StrokeLineJoin
  = LineJoinArcs
  | LineJoinBevel
  | LineJoinMiter
  | LineJoinMiterClip
  | LineJoinRound

derive instance eqStrokeLineJoin :: Eq StrokeLineJoin

instance showStrokeLineJoin :: Show StrokeLineJoin where
  show = case _ of
    LineJoinArcs -> "LineJoinArcs"
    LineJoinBevel -> "LineJoinBevel"
    LineJoinMiter -> "LineJoinMiter"
    LineJoinMiterClip -> "LineJoinMiterClip"
    LineJoinRound -> "LineJoinRound"

printStrokeLineJoin :: StrokeLineJoin -> String
printStrokeLineJoin = case _ of
  LineJoinArcs -> "arcs"
  LineJoinBevel -> "bevel"
  LineJoinMiter -> "miter"
  LineJoinMiterClip -> "miter-clip"
  LineJoinRound -> "round"
