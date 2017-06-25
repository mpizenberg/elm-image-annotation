module Annotation.Geometry.Stroke
    exposing
        ( empty
        , addPoint
        )

{-| Create and manipulate freeline strokes.

@docs empty, addPoint

-}

import Annotation.Geometry.Types exposing (..)
import OpenSolid.Geometry.Types exposing (..)


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
