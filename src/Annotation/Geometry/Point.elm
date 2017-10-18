module Annotation.Geometry.Point
    exposing
        ( fromCoordinates
        )

{-| Create and manipulate points.

@docs fromCoordinates

-}

import Annotation.Geometry.Types exposing (..)
import OpenSolid.Point2d as Point2d exposing (Point2d)


{-| Create a Point with the given coordinates.
-}
fromCoordinates : ( Float, Float ) -> Point
fromCoordinates =
    Point2d.fromCoordinates
