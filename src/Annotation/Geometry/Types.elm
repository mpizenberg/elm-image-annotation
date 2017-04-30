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

@docs Point, BoundingBox, Contour, Outline, Scribble

-}

import Annotation.Geometry.TypesPrivate as Private


{-| A point.
-}
type alias Point =
    Private.Point


{-| A bounding box.
-}
type alias BoundingBox =
    Private.BoundingBox


{-| A contour.
-}
type alias Contour =
    Private.Contour


{-| An outline.
-}
type alias Outline =
    Private.Outline


{-| A scribble.
-}
type alias Scribble =
    Private.Scribble
