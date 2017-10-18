module Annotation.Geometry.Types
    exposing
        ( BoundingBox
        , Contour
        , Outline
        , Point
        , Stroke
        )

{-| Types of the different elements usable for annotations.

In order to initialize an object of one of these types,
look into the corresponding module.
For example, to initialize a `Point`,
you will use the `Point.fromCoordinates` function from the `Point` module.

Rmq: _All those types are aliases of other types
from the [OpenSolid Geometry][OpenSolid] package.
So you can easily extend functionalities based on the Opensolid functions._

[OpenSolid]: http://package.elm-lang.org/packages/opensolid/geometry/2.0.1

@docs Point, BoundingBox, Contour, Outline, Stroke

-}

import OpenSolid.BoundingBox2d exposing (BoundingBox2d)
import OpenSolid.Point2d exposing (Point2d)
import OpenSolid.Polygon2d exposing (Polygon2d)
import OpenSolid.Polyline2d exposing (Polyline2d)


{-| A 2D point.
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

The main difference with a contour is the context of usage.
A contour is more likely to have few identified nodes,
while an outline is more like a continuous line.

-}
type alias Outline =
    Polygon2d


{-| A stroke.
-}
type alias Stroke =
    Polyline2d
