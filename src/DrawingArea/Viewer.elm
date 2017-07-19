module DrawingArea.Viewer
    exposing
        ( Viewer
        , default
          -- UPDATE
        , setSize
        , center
        , centerAt
        , setZoom
        , setZoomCentered
        , zoomIn
        , zoomOut
        , fitImage
        , move
        , positionIn
        , sizeIn
          -- VIEW
        , view
        , innerView
        )

{-| This module provides functions to manage the viewing of the drawing area.


# Model

@docs Viewer, default


# Update

@docs setSize, center, centerAt
@docs setZoom, setZoomCentered, zoomIn, zoomOut
@docs fitImage
@docs move, positionIn, sizeIn


# View

@docs view, innerView

-}

import OpenSolid.Geometry.Types exposing (Frame2d, Point2d(..), Vector2d(..))
import OpenSolid.Frame2d as Frame2d
import OpenSolid.Point2d as Point2d
import OpenSolid.Vector2d as Vector2d
import OpenSolid.Svg as Svg
import Svg exposing (Svg)
import Svg.Lazy exposing (lazy)
import Html exposing (Html)
import Html.Attributes as Attributes
import Image exposing (Image)
import Annotation.Set as Set exposing (Set)


{-| Parameters of the viewer.
-}
type alias Viewer =
    { frame : Frame2d
    , size : Vector2d
    , zoom : Float
    }


{-| Default viewer.
-}
default : Viewer
default =
    { frame = Frame2d.xy
    , size = Vector2d ( 800, 400 )
    , zoom = 1
    }


{-| Resize the viewer.
-}
setSize : ( Float, Float ) -> Viewer -> Viewer
setSize size viewer =
    { viewer | size = Vector2d size }


{-| Compute the center point of the viewing area.
-}
center : Viewer -> Point2d
center viewer =
    Frame2d.originPoint viewer.frame
        |> Point2d.translateBy (Vector2d.scaleBy (0.5 / viewer.zoom) viewer.size)


{-| Recenter the viewing area at a given point.
-}
centerAt : Point2d -> Viewer -> Viewer
centerAt centroid viewer =
    let
        origin =
            centroid
                |> Point2d.translateBy (Vector2d.scaleBy (-0.5 / viewer.zoom) viewer.size)
    in
        { viewer | frame = Frame2d.at origin }


{-| Set the zoom value.
-}
setZoom : Float -> Viewer -> Viewer
setZoom zoom viewer =
    { viewer | zoom = zoom }


{-| Set zoom value while keeping the current center.
-}
setZoomCentered : Float -> Viewer -> Viewer
setZoomCentered zoom viewer =
    let
        currentCenter =
            center viewer
    in
        viewer
            |> setZoom zoom
            |> centerAt currentCenter


{-| Zoom in (x2).
-}
zoomIn : Viewer -> Viewer
zoomIn viewer =
    setZoomCentered (2 * viewer.zoom) viewer


{-| Zoom out (x0.5).
-}
zoomOut : Viewer -> Viewer
zoomOut viewer =
    setZoomCentered (0.5 * viewer.zoom) viewer


{-| Fit the view so that the image takes a certain percentage of its max viewable size.
-}
fitImage : Float -> Image -> Viewer -> Viewer
fitImage ratio image viewer =
    let
        ( vW, vH ) =
            Vector2d.components viewer.size

        ( imW, imH ) =
            ( toFloat image.width, toFloat image.height )

        zoom =
            ratio * min (vW / imW) (vH / imH)
    in
        viewer
            |> setZoom zoom
            |> centerAt (Point2d ( imW / 2, imH / 2 ))


{-| Move the viewer.
-}
move : Vector2d -> Viewer -> Viewer
move vector viewer =
    { viewer
        | frame =
            viewer.frame
                |> Frame2d.translateBy (Vector2d.scaleBy (-1 / viewer.zoom) vector)
    }


{-| Transform coordinates of a point in the frame to their actual coordinates.
-}
positionIn : Viewer -> ( Float, Float ) -> ( Float, Float )
positionIn viewer point =
    Point2d (sizeIn viewer point)
        |> Point2d.placeIn viewer.frame
        |> Point2d.coordinates


{-| Scale a size in the frame to its actual size.
-}
sizeIn : Viewer -> ( Float, Float ) -> ( Float, Float )
sizeIn viewer ( w, h ) =
    ( w / viewer.zoom
    , h / viewer.zoom
    )



-- VIEW ##############################################################


{-| View the svg tag representing the DrawingArea model.
-}
view : List (Html.Attribute msg) -> Viewer -> Svg msg -> Html msg
view attributes viewer svg =
    Html.div
        (Attributes.style [ ( "position", "relative" ) ] :: attributes)
        [ svg ]


{-| Inner Svg tag representing the annotations.
-}
innerView : Viewer -> Maybe Image -> Svg msg -> Svg msg
innerView viewer maybeImage svg =
    let
        innerStyle =
            [ Attributes.style
                [ ( "width", "100%" )
                , ( "height", "100%" )
                , ( "position", "absolute" )
                ]
            ]

        withImage =
            case maybeImage of
                Nothing ->
                    svg

                Just image ->
                    Svg.g [] [ Image.viewSvg [] image, svg ]

        innerSvg =
            withImage
                |> Svg.relativeTo viewer.frame
                |> Svg.scaleAbout Point2d.origin viewer.zoom
    in
        Svg.svg innerStyle [ innerSvg ]
