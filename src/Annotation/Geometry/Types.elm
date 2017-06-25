module Annotation.Geometry.Types
    exposing
        ( Point
        , BoundingBox
        , Contour
        , Outline
        , Scribble
        )

{-| Types of the different elements usable for annotations.

In order to initialize an object of one of these types,
look into the corresponding module.
For example, to initialize a `Point`,
you will use the `Point.fromCoordinates` function from the `Point` module.

Rmq: *All those types are aliases of other types
from the [OpenSolid](http://package.elm-lang.org/packages/opensolid/geometry/latest) package.
So you can easily extend functionalities based on the Opensolid functions.*

@docs Point, BoundingBox, Contour, Outline, Scribble

-}

import OpenSolid.Geometry.Types exposing (..)


{-| A point.
-}
type alias Point =
    Point2d


{-| A bounding box.
-}
type alias BoundingBox =
    BoundingBox2d


{-| A contour.
-}
type alias Contour =
    Polygon2d


{-| An outline.
-}
type alias Outline =
    Polygon2d


{-| A scribble.
-}
type alias Scribble =
    Polyline2d
