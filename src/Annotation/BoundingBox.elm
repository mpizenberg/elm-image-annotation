module Annotation.BoundingBox
    exposing
        ( fromPair
        )

{-| Create and manipulate bounding boxes.

@docs fromPair

-}

import Annotation.Types exposing (..)
import OpenSolid.Point2d as Point2d


{-| Create a bounding box from a pair of points.
-}
fromPair : ( Point, Point ) -> BoundingBox
fromPair ( p1, p2 ) =
    Point2d.hull p1 p2
