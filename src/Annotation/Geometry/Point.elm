module Annotation.Geometry.Point
    exposing
        ( coordinates
        , fromCoordinates
        )

{-| Create and manipulate points.

@docs fromCoordinates, coordinates

-}

import Annotation.Geometry.Types exposing (..)
import OpenSolid.Point2d as Point2d exposing (Point2d)


{-| Create a Point with the given coordinates.
-}
fromCoordinates : ( Float, Float ) -> Point
fromCoordinates =
    Point2d.fromCoordinates


{-| Get the point coordinates.
-}
coordinates : Point -> ( Float, Float )
coordinates =
    Point2d.coordinates
