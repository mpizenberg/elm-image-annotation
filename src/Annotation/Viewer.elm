module Annotation.Viewer
    exposing
        ( Viewer
        , centerAt
        , default
        , fitImage
        , getCenter
        , grabMove
        , placeIn
        , positionIn
        , setSize
        , setZoom
        , setZoomCentered
        , sizeIn
        , viewIn
        , viewInWithDetails
        , zoomIn
        , zoomOut
        )

{-| This module provides functions to manage the viewing area.


# Model

@docs Viewer, default


# Update

@docs setSize, getCenter, centerAt
@docs setZoom, setZoomCentered, zoomIn, zoomOut
@docs fitImage
@docs grabMove, positionIn, sizeIn


# View

@docs viewIn, viewInWithDetails, placeIn

-}

import Annotation.Geometry.Types exposing (..)
import Html exposing (Html)
import Html.Attributes as HtmlA
import Image exposing (Image)
import OpenSolid.Frame2d as Frame2d exposing (Frame2d)
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.Svg as Svg
import OpenSolid.Vector2d as Vector2d
import Svg exposing (Svg)


{-| Parameters of the viewer.
-}
type alias Viewer =
    { frame : Frame2d
    , size : ( Float, Float )
    , zoom : Float
    }


{-| Default viewer.
-}
default : Viewer
default =
    { frame = Frame2d.xy
    , size = ( 800, 400 )
    , zoom = 1
    }


{-| Reset size of the viewer.
-}
setSize : ( Float, Float ) -> Viewer -> Viewer
setSize size viewer =
    { viewer | size = size }


{-| Compute the center point of the viewer.
-}
getCenter : Viewer -> Point
getCenter viewer =
    let
        sizeVector =
            Vector2d.fromComponents viewer.size

        translationVector =
            Vector2d.scaleBy (0.5 / viewer.zoom) sizeVector
    in
    Frame2d.originPoint viewer.frame
        |> Point2d.translateBy translationVector


{-| Recenter the viewing area at a given point.
-}
centerAt : Point2d -> Viewer -> Viewer
centerAt center viewer =
    let
        sizeVector =
            Vector2d.fromComponents viewer.size

        translationVector =
            Vector2d.scaleBy (0.5 / viewer.zoom) sizeVector

        newOrigin =
            Point2d.translateBy translationVector center
    in
    { viewer | frame = Frame2d.atPoint newOrigin }


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
            getCenter viewer
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
            viewer.size

        ( imW, imH ) =
            ( toFloat image.width, toFloat image.height )

        zoom =
            ratio * min (vW / imW) (vH / imH)
    in
    viewer
        |> setZoom zoom
        |> centerAt (Point2d.fromCoordinates ( imW / 2, imH / 2 ))


{-| Translate the viewer frame opposite to the vector (used for "grab and move").
-}
grabMove : ( Float, Float ) -> Viewer -> Viewer
grabMove movement viewer =
    let
        translationVector =
            Vector2d.scaleBy (-1 / viewer.zoom) (Vector2d.fromComponents movement)
    in
    { viewer | frame = Frame2d.translateBy translationVector viewer.frame }


{-| Transform coordinates of a point in the frame to their actual image coordinates.
-}
positionIn : Viewer -> ( Float, Float ) -> ( Float, Float )
positionIn viewer point =
    Point2d.fromCoordinates (sizeIn viewer point)
        |> Point2d.placeIn viewer.frame
        |> Point2d.coordinates


{-| Scale a size in the frame to its actual image size.
-}
sizeIn : Viewer -> ( Float, Float ) -> ( Float, Float )
sizeIn viewer ( w, h ) =
    ( w / viewer.zoom
    , h / viewer.zoom
    )



-- VIEW ##############################################################


{-| Place an svg element in a viewer (apply the appropriate frame transformation).

This is for a user who wants to keep full control of the viewing process.
The result can then be used as a regular svg element.

It does not compute new coordinates of each svg element,
but simply embeds them in a group with the appropriate transformation.
So try using it only once, on a group containing all elements to insert.

For simple viewing, prefer using the `viewIn` function.

-}
placeIn : Viewer -> Svg msg -> Svg msg
placeIn viewer svg =
    Svg.relativeTo viewer.frame svg
        |> Svg.scaleAbout Point2d.origin viewer.zoom


{-| View the generated svg annotations (bounding boxes, contours, ...)
in the viewer.

For display of the corresponding image in background,
simply group it with the annotations to generate a new Svg msg:

    Svg.g [] [ Image.viewSvg [] image, svgAnnotations ]

Be aware that the output of `viewIn` is an `svg` tag embedded in a `div` tag.
Embedding is performed to remedy some inconsistencies that may occur when
trying to get the dimensions of an `svg` tag with `clientWidth`.
(see [W3C spec](https://www.w3.org/TR/cssom-view-1/#dom-element-clientwidth):
"If the element has no associated CSS layout box
or if the CSS layout box is inline, return zero.")

For finer-grained control, prefer using `placeIn`.

-}
viewIn : Viewer -> Svg msg -> Html msg
viewIn =
    viewInWithDetails []


{-| Same as `viewIn` but html attributes can be added (like class, ...).
-}
viewInWithDetails : List (Html.Attribute msg) -> Viewer -> Svg msg -> Html msg
viewInWithDetails htmlAttributes viewer svg =
    [ Svg.svg [ HtmlA.style [ ( "flex", "1" ) ] ] [ placeIn viewer svg ] ]
        |> Html.div (HtmlA.style [ ( "display", "flex" ) ] :: htmlAttributes)
