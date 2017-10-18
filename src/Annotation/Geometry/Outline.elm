module Annotation.Geometry.Outline
    exposing
        ( empty
        , fromPoints
        )

{-| Create and manipulate free line closed outlines.

The `Outline` type is an alias for the [OpenSolid Geometry Polygon2d][polygon2d]
type so if a functionality is not provided by this module,
you can easily extend it with functions from the OpenSolid/Geometry package.

[polygon2d]: http://package.elm-lang.org/packages/opensolid/geometry/2.0.1/OpenSolid-Polygon2d#Polygon2d

@docs empty, fromPoints

-}

import Annotation.Geometry.Types exposing (..)
import OpenSolid.Polygon2d as Polygon2d exposing (Polygon2d)


{-| Create an empty outline.
-}
empty : Outline
empty =
    fromPoints []


{-| Create a closed outline from a list of points.
-}
fromPoints : List Point -> Outline
fromPoints =
    Polygon2d.fromVertices
