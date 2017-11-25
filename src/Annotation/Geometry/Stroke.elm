module Annotation.Geometry.Stroke
    exposing
        ( addPoint
        , close
        , empty
        , fromPoints
        , points
        )

{-| Create and manipulate free line strokes.

The stroke type is an alias for the [OpenSolid Geometry Polyline2d][polyline2d]
type so if a functionality is not provided by this module,
you can easily extend it with functions from the OpenSolid/Geometry package.

[polyline2d]: http://package.elm-lang.org/packages/opensolid/geometry/2.0.1/OpenSolid-Polyline2d#Polyline2d

@docs empty, addPoint, fromPoints, points, close

-}

import Annotation.Geometry.Contour as Contour
import Annotation.Geometry.Types exposing (..)
import OpenSolid.Polyline2d as Polyline2d exposing (Polyline2d)


{-| Create an empty stroke.
-}
empty : Stroke
empty =
    fromPoints []


{-| Add a point to an unfinished stroke.
-}
addPoint : Point -> Stroke -> Stroke
addPoint point stroke =
    fromPoints (point :: points stroke)


{-| Create a stroke from a list of points
-}
fromPoints : List Point -> Stroke
fromPoints =
    Polyline2d.fromVertices


{-| Retrieve the list of points of the stroke.
-}
points : Stroke -> List Point
points =
    Polyline2d.vertices


{-| Close a stroke (polyline) into a polygon.

The output can be interpreted as a either a `Contour` or an `Outline`
since both are OpenSolid polygons type aliases.

-}
close : Stroke -> Contour
close stroke =
    Contour.fromPoints (Polyline2d.vertices stroke)
