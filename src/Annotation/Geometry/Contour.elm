module Annotation.Geometry.Contour
    exposing
        ( empty
        , encode
        , fromPoints
        , points
        )

{-| Create and manipulate polygonal contours.

The `Contour` type is an alias for the [OpenSolid Geometry Polygon2d][polygon2d]
type so if a functionality is not provided by this module,
you can easily extend it with functions from the OpenSolid/Geometry package.

[polygon2d]: http://package.elm-lang.org/packages/opensolid/geometry/2.0.1/OpenSolid-Polygon2d#Polygon2d

@docs empty, fromPoints, points, encode

-}

import Annotation.Geometry.Types exposing (..)
import Json.Encode as Encode exposing (Value)
import OpenSolid.Geometry.Encode as Encode
import OpenSolid.Polygon2d as Polygon2d exposing (Polygon2d)


{-| Create an empty contour.
-}
empty : Contour
empty =
    fromPoints []


{-| Create a contour from a list of points.
-}
fromPoints : List Point -> Contour
fromPoints =
    Polygon2d.fromVertices


{-| Retrieve the list of points of the contour.
-}
points : Contour -> List Point
points =
    Polygon2d.vertices


{-| Default encoder.
-}
encode : Contour -> Value
encode =
    Encode.polygon2d
