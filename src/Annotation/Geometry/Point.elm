module Annotation.Geometry.Point
    exposing
        ( coordinates
        , encode
        , fromCoordinates
        )

{-| Create and manipulate points.

@docs fromCoordinates, coordinates, encode

-}

import Annotation.Geometry.Types exposing (..)
import Json.Encode as Encode exposing (Value)
import OpenSolid.Geometry.Encode as Encode
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


{-| Default encoder.
-}
encode : Point -> Value
encode =
    Encode.point2d
