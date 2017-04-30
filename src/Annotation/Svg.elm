module Annotation.Svg
    exposing
        ( pointWithDetails
        , pointWithStyle
        , point
        )

{-| View the annotations as Svg.

@docs pointWithDetails, pointWithStyle, point

-}

import Annotation.Geometry.Types exposing (..)
import OpenSolid.Geometry.Types exposing (..)
import Annotation.Style as Style
import Annotation.Color as Color exposing (Color)
import Svg exposing (Svg, Attribute)
import OpenSolid.Svg as Svg
import Svg.Attributes as SvgA


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
pointWithStyle : Style.Point -> Point -> Svg msg
pointWithStyle style point =
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
    pointWithStyle Style.pointDefault
