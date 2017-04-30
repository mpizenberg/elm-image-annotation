module Annotation.Geometry.Outline
    exposing
        ( empty
        , addPoint
        )

{-| Create and manipulate freeline closed outlines.

@docs empty, addPoint

-}

import Annotation.Geometry.Types exposing (..)
import OpenSolid.Geometry.Types exposing (..)


{-| Create an empty outline.
-}
empty : Outline
empty =
    Polygon2d []


{-| Add a point to an unfinished outline.
-}
addPoint : Point -> Outline -> Outline
addPoint point outline =
    case outline of
        Polygon2d outlinePoints ->
            Polygon2d (point :: outlinePoints)
