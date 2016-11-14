-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module SvgViewer
    exposing
        ( Option
        , OptionSet
        , defaultOptions
        , SvgViewer
        , default
          -- UPDATE
        , changeBgImage
        , fitImage
        , optionValue
        , changeOption
        , resize
        , changeZoom
        , changeZoomCentered
        , zoomIn
        , zoomOut
        , currentCenter
        , reCenter
        , move
        , transformPos
        , transformSize
          -- VIEW
        , view
        )

{-| The viewer of a drawing area.
It holds all the geometric properties of the view.

# Model
@docs Option, OptionSet, defaultOptions, SvgViewer, default

# Update
@docs changeBgImage, fitImage
@docs optionValue, changeOption
@docs resize, changeZoom, changeZoomCentered, zoomIn, zoomOut
@docs currentCenter, reCenter, move
@docs transformPos, transformSize

# View
@docs view
-}

import Dict exposing (Dict)
import Array exposing (Array)
import Svg exposing (Svg)
import Svg.Attributes as SvgA
import Html as H exposing (Html)
import Html.Attributes as HA
import Image exposing (Image)
import AnnotationSet as AnnSet exposing (AnnotationSet)


-- MODEL #############################################################


{-| One option (a string giving its name).
-}
type alias Option =
    String


{-| Regrouping off the viewer options.
-}
type alias OptionSet =
    Dict Option Bool


{-| The default options.
-}
defaultOptions : OptionSet
defaultOptions =
    Dict.fromList
        []


{-| An Svg viewer has a background image and some geometric properties.
-}
type alias SvgViewer =
    { bgImage : Maybe Image
    , options : OptionSet
    , size : ( Float, Float )
    , origin : ( Float, Float )
    , zoom : Float
    }


{-| The default Svg viewer for a drawing area.
-}
default : SvgViewer
default =
    { bgImage = Nothing
    , options = defaultOptions
    , size = ( 800, 400 )
    , origin = ( 0, 0 )
    , zoom = 1
    }



-- UPDATE ############################################################


{-| Change the background image.
-}
changeBgImage : Maybe Image -> SvgViewer -> SvgViewer
changeBgImage maybeImage viewer =
    { viewer | bgImage = maybeImage }


{-| Fit the view so that the image takes a certain percentage of its max viewable size.
-}
fitImage : Float -> SvgViewer -> SvgViewer
fitImage ratio viewer =
    case viewer.bgImage of
        Nothing ->
            viewer

        Just image ->
            let
                ( vW, vH ) =
                    viewer.size

                ( imW, imH ) =
                    ( toFloat image.width, toFloat image.height )

                zoom =
                    ratio * min (vW / imW) (vH / imH)

                origin =
                    ( 0.5 * (imW - vW / zoom)
                    , 0.5 * (imH - vH / zoom)
                    )
            in
                { viewer
                    | bgImage = Just image
                    , origin = origin
                    , zoom = zoom
                }


{-| Get the value of an option.
-}
optionValue : Option -> SvgViewer -> Bool
optionValue option viewer =
    case Dict.get option viewer.options of
        Nothing ->
            False

        Just value ->
            value


{-| Change an option of the viewer.
-}
changeOption : Option -> Bool -> SvgViewer -> SvgViewer
changeOption option value viewer =
    { viewer | options = Dict.insert option value viewer.options }


{-| Resize the viewer.
-}
resize : ( Float, Float ) -> SvgViewer -> SvgViewer
resize size viewer =
    { viewer | size = size }


{-| Change and zoom value.
-}
changeZoom : Float -> SvgViewer -> SvgViewer
changeZoom zoom viewer =
    { viewer | zoom = zoom }


{-| Change and zoom value while keeping the current center.
-}
changeZoomCentered : Float -> SvgViewer -> SvgViewer
changeZoomCentered zoom viewer =
    let
        center =
            currentCenter viewer
    in
        { viewer | zoom = zoom }
            |> reCenter center


{-| Zoom in (x2).
-}
zoomIn : SvgViewer -> SvgViewer
zoomIn viewer =
    changeZoomCentered (2 * viewer.zoom) viewer


{-| Zoom out (x0.5).
-}
zoomOut : SvgViewer -> SvgViewer
zoomOut viewer =
    changeZoomCentered (0.5 * viewer.zoom) viewer


{-| Get the current center of the viewer.
-}
currentCenter : SvgViewer -> ( Float, Float )
currentCenter viewer =
    let
        ( left, top ) =
            viewer.origin

        ( width, height ) =
            viewer.size
    in
        ( left + 0.5 * width / viewer.zoom
        , top + 0.5 * height / viewer.zoom
        )


{-| Recenter the viewer at a given point.
-}
reCenter : ( Float, Float ) -> SvgViewer -> SvgViewer
reCenter ( x, y ) viewer =
    let
        ( width, height ) =
            viewer.size
    in
        { viewer
            | origin =
                ( x - 0.5 * width / viewer.zoom
                , y - 0.5 * height / viewer.zoom
                )
        }


{-| Move the viewer.
-}
move : ( Float, Float ) -> SvgViewer -> SvgViewer
move ( moveX, moveY ) viewer =
    let
        ( left, top ) =
            viewer.origin
    in
        { viewer
            | origin =
                ( left - moveX / viewer.zoom
                , top - moveY / viewer.zoom
                )
        }


{-| Transform coordinates of a position.
-}
transformPos : SvgViewer -> ( Float, Float ) -> ( Int, Int )
transformPos viewer ( x, y ) =
    let
        ( left, top ) =
            viewer.origin
    in
        ( round <| left + x / viewer.zoom
        , round <| top + y / viewer.zoom
        )


{-| Transform coordinates of a size.
-}
transformSize : SvgViewer -> ( Float, Float ) -> ( Int, Int )
transformSize viewer ( width, height ) =
    ( round <| width / viewer.zoom
    , round <| height / viewer.zoom
    )



-- VIEW ##############################################################


{-| View the svg tag representing the DrawingArea model
-}
view : List (H.Attribute msg) -> AnnotationSet -> SvgViewer -> Html msg
view attributes set viewer =
    H.div
        (sizeStyleAttribute viewer :: attributes)
        [ Svg.svg
            [ HA.style
                [ ( "width", "100%" )
                , ( "height", "100%" )
                , ( "display", "block" )
                ]
            ]
            [ Svg.g
                [ svgTransform viewer.zoom viewer.origin ]
                ((case viewer.bgImage of
                    Nothing ->
                        []

                    Just image ->
                        [ Image.viewSvg [] Nothing image ]
                 )
                    ++ AnnSet.viewAllSelections set
                )
            ]
        ]



-- VIEW HELPERS ######################################################


svgTransform : Float -> ( Float, Float ) -> Svg.Attribute msg
svgTransform zoomLevel ( x, y ) =
    SvgA.transform <|
        "scale("
            ++ toString zoomLevel
            ++ ") "
            ++ "translate"
            ++ toString ( -x, -y )


sizeStyleAttribute : SvgViewer -> Svg.Attribute msg
sizeStyleAttribute viewer =
    let
        ( width, height ) =
            viewer.size
    in
        HA.style
            [ ( "width", toString width ++ "px" )
            , ( "height", toString height ++ "px" )
            ]
