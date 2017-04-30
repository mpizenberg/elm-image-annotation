module Annotation.Geometry.BoundingBox
    exposing
        ( fromPair
        , attributes
        )

{-| Create and manipulate bounding boxes.

@docs fromPair, attributes

-}

import Annotation.Geometry.Types exposing (..)
import OpenSolid.Point2d as Point2d
import OpenSolid.BoundingBox2d as BoundingBox2d
import Svg
import Svg.Attributes as SvgA


{-| Create a bounding box from a pair of points.
-}
fromPair : ( Point, Point ) -> BoundingBox
fromPair ( p1, p2 ) =
    Point2d.hull p1 p2


{-| Get the geometric svg attributes of a bounding box.
-}
attributes : BoundingBox -> List (Svg.Attribute msg)
attributes bbox =
    let
        ( left, top ) =
            ( BoundingBox2d.minX bbox
            , BoundingBox2d.minY bbox
            )

        ( width, height ) =
            BoundingBox2d.dimensions bbox
    in
        [ SvgA.x (toString left)
        , SvgA.y (toString top)
        , SvgA.width (toString width)
        , SvgA.height (toString height)
        ]
