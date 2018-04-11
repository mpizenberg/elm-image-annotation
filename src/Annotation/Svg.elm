module Annotation.Svg
    exposing
        ( boundingBox
        , boundingBoxStyled
        , boundingBoxWithDetails
        , contour
        , contourStyled
        , contourWithDetails
        , outline
        , outlineStyled
        , outlineWithDetails
        , point
        , pointStyled
        , pointWithDetails
        , stroke
        , strokeStyled
        , strokeWithDetails
        )

{-| View the annotations as Svg.


# Simple svg rendering with default styles

@docs point, boundingBox, stroke, contour, outline


# Svg rendering with personal styles

@docs pointStyled, boundingBoxStyled, strokeStyled, contourStyled, outlineStyled


# Detailed rendering with whatever svg attributes

@docs pointWithDetails, boundingBoxWithDetails, strokeWithDetails, contourWithDetails, outlineWithDetails

-}

import Annotation.Color as Color
import Annotation.Geometry.BoundingBox as BoundingBox
import Annotation.Geometry.Types exposing (..)
import Annotation.Style as Style
import OpenSolid.Circle2d as Circle2d
import OpenSolid.Point2d as Point2d
import OpenSolid.Svg as Svg
import Svg exposing (Attribute, Svg)
import Svg.Attributes as SvgA


-- POINT #############################################################


{-| Svg detailed view of a point.

Specify in the attributes the radius and styling of the disk
used to represent the point. Alternatively, put a class attribute
and set those in a style sheet (CSS).

The attributes arguments are appended to the already provided ones
which are the circle center point coordinates (`cx`, `cy`).

Most often, you will also want to add `pointerEvents "none"`
to prevent unwanted pointer events from happening.
It is added by default in the `point` and `pointStyled` functions
but not in `pointWithDetails`.

-}
pointWithDetails : List (Attribute msg) -> Point -> Svg msg
pointWithDetails attributes point =
    let
        ( x, y ) =
            Point2d.coordinates point
    in
    [ SvgA.cx (toString x), SvgA.cy (toString y), SvgA.pointerEvents "none" ]
        |> List.append attributes
        |> flip Svg.circle []


{-| Svg view of a point with given style.
-}
pointStyled : Style.Line -> Style.Fill -> Float -> Point -> Svg msg
pointStyled lineStyle fillStyle radius point =
    SvgA.pointerEvents "none"
        :: Style.fillAttribute fillStyle
        :: Style.strokeAttributes lineStyle
        |> flip Svg.circle2d (Circle2d.with { centerPoint = point, radius = radius })


{-| Svg view of a point with default style.
Default style is defined in `Annotation.Style`.
-}
point : Point -> Svg msg
point =
    pointStyled Style.strokeDefault Style.fillDefault 30



-- BOUNDING BOX ######################################################


{-| Svg detailed view of a bounding box.

Specify in the attributes the style of the bounding box.
Alternatively put a class attribute and set those in a style sheet (CSS).

The attributes arguments are appended to the already provided ones
which are the geometric attributes (position and size) of the bounding box.

Most often, you will also want to add `pointerEvents "none"`
to prevent unwanted pointer events from happening.
It is added by default in the `boundingBox` and `boundingBoxStyled`
functions but not in `boundingBoxWithDetails`.

-}
boundingBoxWithDetails : List (Attribute msg) -> BoundingBox -> Svg msg
boundingBoxWithDetails attributes bbox =
    (BoundingBox.svgAttributes bbox ++ attributes)
        |> flip Svg.rect []


{-| Svg view of a bounding box with given style.
-}
boundingBoxStyled : Style.Line -> Style.Fill -> BoundingBox -> Svg msg
boundingBoxStyled lineStyle fillStyle bbox =
    SvgA.pointerEvents "none"
        :: BoundingBox.svgAttributes bbox
        ++ (Style.fillAttribute fillStyle :: Style.strokeAttributes lineStyle)
        |> flip Svg.rect []


{-| Svg view of a bounding box with default style.
Default style is defined in `Annotation.Style`.
-}
boundingBox : BoundingBox -> Svg msg
boundingBox =
    boundingBoxStyled Style.strokeDefault Style.fillDefault



-- STROKE ############################################################


{-| Svg detailed view of a stroke.

Specify in the attributes the style of the stroke.
Alternatively put a class attribute and set those in a style sheet (CSS).

The attributes arguments are appended to the already provided ones
which are the geometric attributes (line points positions).

Most often, you will also want to add `pointerEvents "none"`
to prevent unwanted pointer events from happening.
It is added by default in the `stroke` and `strokeStyled`
functions but not in `strokeWithDetails`.

-}
strokeWithDetails : List (Attribute msg) -> Stroke -> Svg msg
strokeWithDetails =
    Svg.polyline2d


{-| Svg view of a stroke with given style.
-}
strokeStyled : Style.Line -> Stroke -> Svg msg
strokeStyled lineStyle =
    SvgA.pointerEvents "none"
        :: Style.fillAttribute Style.NoFill
        :: Style.strokeAttributes lineStyle
        |> Svg.polyline2d


{-| Svg view of a stroke with default style.
Default style is defined in `Annotation.Style`.
-}
stroke : Stroke -> Svg msg
stroke =
    strokeStyled Style.strokeDefault



-- CONTOUR ###########################################################


{-| Svg detailed view of a contour.

Specify in the attributes the style of the contour.
Alternatively put a class attribute and set those in a style sheet (CSS).

The attributes arguments are appended to the already provided ones
which are the geometric attributes (contour points positions).

Most often, you will also want to add `pointerEvents "none"`
to prevent unwanted pointer events from happening.
It is added by default in the `contour` and `contourStyled`
functions but not in `contourWithDetails`.

-}
contourWithDetails : List (Attribute msg) -> Contour -> Svg msg
contourWithDetails =
    Svg.polygon2d


{-| Svg view of a contour with given style.
-}
contourStyled : Style.Line -> Style.Fill -> Contour -> Svg msg
contourStyled lineStyle fillStyle =
    SvgA.pointerEvents "none"
        :: Style.fillAttribute fillStyle
        :: Style.strokeAttributes lineStyle
        |> Svg.polygon2d


{-| Svg view of a contour with default style.
Default style is defined in `Annotation.Style`.
-}
contour : Contour -> Svg msg
contour =
    contourStyled Style.strokeDefault Style.NoFill



-- OUTLINE ###########################################################


{-| Svg detailed view of an outline.

Specify in the attributes the style of the outline.
Alternatively put a class attribute and set those in a style sheet (CSS).

The attributes arguments are appended to the already provided ones
which are the geometric attributes (outline points positions).

Most often, you will also want to add `pointerEvents "none"`
to prevent unwanted pointer events from happening.
It is added by default in the `outline` and `outlineStyled`
functions but not in `outlineWithDetails`.

-}
outlineWithDetails : List (Attribute msg) -> Outline -> Svg msg
outlineWithDetails =
    Svg.polygon2d


{-| Svg view of an outline with given style.
-}
outlineStyled : Style.Line -> Style.Fill -> Outline -> Svg msg
outlineStyled lineStyle fillStyle =
    SvgA.pointerEvents "none"
        :: Style.fillAttribute fillStyle
        :: Style.strokeAttributes lineStyle
        |> Svg.polygon2d


{-| Svg view of an outline with default style.
Default style is defined in `Annotation.Style`.
-}
outline : Outline -> Svg msg
outline =
    outlineStyled Style.strokeDefault Style.NoFill
