module Annotation.Scribble
    exposing
        ( empty
        , addPoint
        )

{-| Create and manipulate freeline scribbles.

@docs empty, addPoint

-}

import Annotation.Types exposing (..)
import OpenSolid.Geometry.Types exposing (..)


{-| Create an empty scribble.
-}
empty : Scribble
empty =
    Polyline2d []


{-| Add a point to an unfinished scribble.
-}
addPoint : Point -> Scribble -> Scribble
addPoint point scribble =
    case scribble of
        Polyline2d scribblePoints ->
            Polyline2d (point :: scribblePoints)
