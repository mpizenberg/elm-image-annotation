module Annotation.Geometry.Contour
    exposing
        ( empty
        , addPoint
        , fromPoints
        )

{-| Create and manipulate polygonal contours.

@docs empty, addPoint, fromPoints

-}

import Annotation.Geometry.Types exposing (..)
import OpenSolid.Geometry.Types exposing (..)


{-| Create an empty contour.
-}
empty : Contour
empty =
    Polygon2d []


{-| Add a point to an unfinished contour.
-}
addPoint : Point -> Contour -> Contour
addPoint point contour =
    case contour of
        Polygon2d contourPoints ->
            Polygon2d (point :: contourPoints)


{-| Create a contour from a list of points.
-}
fromPoints : List Point -> Contour
fromPoints =
    Polygon2d
