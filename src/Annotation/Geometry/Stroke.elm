module Annotation.Geometry.Stroke
    exposing
        ( empty
        , addPoint
        , close
        )

{-| Create and manipulate freeline strokes.

@docs empty, addPoint, close

-}

import Annotation.Geometry.Types exposing (..)
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Polyline2d as Polyline2d


{-| Create an empty stroke.
-}
empty : Stroke
empty =
    Polyline2d []


{-| Add a point to an unfinished stroke.
-}
addPoint : Point -> Stroke -> Stroke
addPoint point stroke =
    case stroke of
        Polyline2d strokePoints ->
            Polyline2d (point :: strokePoints)


{-| Close a stroke (polyline) into a polygon.
The output can be interpreted as a `Contour` or `Outline`
since both are polygons.
-}
close : Stroke -> Polygon2d
close stroke =
    Polygon2d (Polyline2d.vertices stroke)
