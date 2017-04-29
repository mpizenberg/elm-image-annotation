module Annotation.Point
    exposing
        ( fromCoordinates
        )

{-| Create and manipulate points.

@docs fromCoordinates

-}

import Annotation.Types exposing (..)
import OpenSolid.Geometry.Types exposing (..)


{-| Create a Point with the given coordinates.
-}
fromCoordinates : ( Float, Float ) -> Point
fromCoordinates coordinates =
    Point2d coordinates
