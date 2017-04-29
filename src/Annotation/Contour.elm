module Annotation.Contour
    exposing
        ( empty
        , addPoint
        )

{-| Create and manipulate polygonal contours.

@docs empty, addPoint

-}

import Annotation.Types exposing (..)
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
