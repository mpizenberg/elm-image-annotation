module Annotation.Geometry.BoundingBox
    exposing
        ( encode
        , fromPair
        , svgAttributes
        )

{-| Create and manipulate bounding boxes.

@docs fromPair, svgAttributes, encode

-}

import Annotation.Geometry.Types exposing (..)
import Json.Encode as Encode exposing (Value)
import OpenSolid.BoundingBox2d as BoundingBox2d
import OpenSolid.Geometry.Encode as Encode
import OpenSolid.Point2d as Point2d
import Svg
import Svg.Attributes as SvgA


{-| Create a bounding box from a pair of points
representing opposite corners of the bounding box.
-}
fromPair : ( Point, Point ) -> BoundingBox
fromPair ( p1, p2 ) =
    Point2d.hull p1 p2


{-| Get the geometric svg attributes of a bounding box.
-}
svgAttributes : BoundingBox -> List (Svg.Attribute msg)
svgAttributes bbox =
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


{-| Default encoder.
-}
encode : BoundingBox -> Value
encode =
    Encode.boundingBox2d
