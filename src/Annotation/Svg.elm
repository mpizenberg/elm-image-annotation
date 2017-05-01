module Annotation.Svg
    exposing
        ( pointWithDetails
        , pointStyled
        , point
        , boundingBoxWithDetails
        , boundingBoxStyled
        , boundingBox
        , contourWithDetails
        , contourStyled
        , contour
        , contourBuildingWithDetails
        , contourBuildingStyled
        , contourBuilding
        )

{-| View the annotations as Svg.

@docs point, pointStyled, pointWithDetails

@docs boundingBox, boundingBoxStyled, boundingBoxWithDetails

@docs contour, contourStyled, contourWithDetails
@docs contourBuilding, contourBuildingStyled, contourBuildingWithDetails

-}

import OpenSolid.Geometry.Types exposing (..)
import Annotation.Geometry.Types exposing (..)
import Annotation.Geometry.BoundingBox as BoundingBox
import OpenSolid.Polygon2d as Polygon2d
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



-- CONTOUR ###########################################################


{-| Svg detailed view of a contour.

Specify in the attributes the style of the contour.
Alternatively put a class attribute and set those in a style sheet (CSS).

-}
contourWithDetails : List (Attribute msg) -> Contour -> Svg msg
contourWithDetails =
    Svg.polygon2d


{-| Svg view of a contour with given style.
-}
contourStyled : Style.Line -> Style.Fill -> Contour -> Svg msg
contourStyled lineStyle fillStyle contour =
    let
        styleAttributes =
            Style.strokeAttributes lineStyle ++ Style.fillAttributes fillStyle
    in
        Svg.polygon2d styleAttributes contour


{-| Svg view of a contour with default style.
-}
contour : Contour -> Svg msg
contour =
    contourStyled Style.strokeDefault Style.NoFill


{-| Svg detailed view of a contour being built with a temporary new point.

Specify in the attributes the style of the contour.
Alternatively put a class attribute and set those in a style sheet (CSS).

-}
contourBuildingWithDetails : List (Attribute msg) -> Point -> Contour -> Svg msg
contourBuildingWithDetails attributes newPoint contour =
    Polyline2d (newPoint :: Polygon2d.vertices contour)
        |> Svg.polyline2d attributes


{-| Svg view of a contour with a temporary new point with given style.
-}
contourBuildingStyled : Style.Line -> Style.Fill -> Point -> Contour -> Svg msg
contourBuildingStyled lineStyle fillStyle newPoint contour =
    let
        styleAttributes =
            Style.strokeAttributes lineStyle ++ Style.fillAttributes fillStyle

        polyline2d =
            Polyline2d (newPoint :: Polygon2d.vertices contour)
    in
        Svg.polyline2d styleAttributes polyline2d


{-| Svg view of a contour with a temporary new point with default style.
-}
contourBuilding : Point -> Contour -> Svg msg
contourBuilding =
    contourBuildingStyled Style.strokeDefault Style.NoFill
