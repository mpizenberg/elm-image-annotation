module Annotation.Svg
    exposing
        ( pointWithDetails
        , pointStyled
        , point
        , boundingBoxWithDetails
        , boundingBoxStyled
        , boundingBox
        )

{-| View the annotations as Svg.

@docs point, pointStyled, pointWithDetails

@docs boundingBox, boundingBoxStyled, boundingBoxWithDetails

-}

import OpenSolid.Geometry.Types exposing (..)
import Annotation.Geometry.Types exposing (..)
import Annotation.Geometry.BoundingBox as BoundingBox
import Annotation.Style as Style
import Annotation.Color as Color exposing (Color)
import Svg exposing (Svg, Attribute)
import OpenSolid.Svg as Svg
import Svg.Attributes as SvgA


-- POINT #############################################################


{-| Svg detailed view of a point.

Specify in the attributes the radius and styling of the circle
used to represent the point. Alternatively, put a class attribute
and set those in a style sheet (CSS).

-}
pointWithDetails : List (Attribute msg) -> Point -> Svg msg
pointWithDetails attributes (Point2d ( x, y )) =
    [ SvgA.cx (toString x), SvgA.cy (toString y) ]
        |> List.append attributes
        |> flip Svg.circle []


{-| Svg view of a point with given style.
-}
pointStyled : Style.Point -> Point -> Svg msg
pointStyled style point =
    case style of
        Style.NoPoint ->
            Svg.text "Hidden point"

        Style.Disk radius color ->
            Circle2d { centerPoint = point, radius = radius }
                |> Svg.circle2d [ SvgA.fill (Color.toStr color) ]


{-| Svg view of a point with default style.
-}
point : Point -> Svg msg
point =
    pointStyled Style.pointDefault



-- BOUNDING BOX ######################################################


{-| Svg detailed view of a bounding box.

Specify in the attributes the style of the bounding box.
Alternatively put a class attribute and set those in a style sheet (CSS).

-}
boundingBoxWithDetails : List (Attribute msg) -> BoundingBox -> Svg msg
boundingBoxWithDetails attributes bbox =
    BoundingBox.attributes bbox
        ++ attributes
        |> flip Svg.rect []


{-| Svg view of a bounding box with given style.
-}
boundingBoxStyled : Style.Line -> Style.Fill -> BoundingBox -> Svg msg
boundingBoxStyled lineStyle fillStyle bbox =
    BoundingBox.attributes bbox
        ++ Style.strokeAttributes lineStyle
        ++ Style.fillAttributes fillStyle
        |> flip Svg.rect []


{-| Svg view of a bounding box with default style.
-}
boundingBox : BoundingBox -> Svg msg
boundingBox =
    boundingBoxStyled Style.strokeDefault Style.NoFill
