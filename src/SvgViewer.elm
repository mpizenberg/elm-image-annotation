module SvgViewer exposing (..)

import Dict exposing (Dict)
import Array exposing (Array)
import Svg exposing (Svg)
import Svg.Attributes as SvgA
import Html.Attributes as HA
import Image exposing (Image)
import AnnotationSet as AnnSet exposing (AnnotationSet)
import Pointer exposing (Pointer)


-- MODEL #############################################################


type alias Option =
    String


type alias OptionSet =
    Dict Option Bool


{-| The default options.
-}
defaultOptions : OptionSet
defaultOptions =
    Dict.fromList
        [ ( "LastAnnotationOnly", True )
        ]


type alias SvgViewer =
    { bgImage : Maybe Image
    , options : OptionSet
    , size : ( Float, Float )
    , origin : ( Float, Float )
    , zoom : Float
    }


default : SvgViewer
default =
    { bgImage = Nothing
    , options = defaultOptions
    , size = ( 800, 400 )
    , origin = ( 0, 0 )
    , zoom = 1
    }



-- UPDATE ############################################################


changeBgImage : Maybe Image -> SvgViewer -> SvgViewer
changeBgImage maybeImage viewer =
    { viewer | bgImage = maybeImage }


optionValue : Option -> SvgViewer -> Bool
optionValue option viewer =
    case Dict.get option viewer.options of
        Nothing ->
            False

        Just value ->
            value


changeOption : Option -> Bool -> SvgViewer -> SvgViewer
changeOption option value viewer =
    { viewer | options = Dict.insert option value viewer.options }


resize : ( Float, Float ) -> SvgViewer -> SvgViewer
resize size viewer =
    { viewer | size = size }


changeZoom : Float -> SvgViewer -> SvgViewer
changeZoom zoom viewer =
    { viewer | zoom = zoom }


zoomIn : SvgViewer -> SvgViewer
zoomIn viewer =
    changeZoom (2 * viewer.zoom) viewer


zoomOut : SvgViewer -> SvgViewer
zoomOut viewer =
    changeZoom (0.5 * viewer.zoom) viewer


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


update : Pointer -> SvgViewer -> SvgViewer
update pointer viewer =
    let
        ( left, top ) =
            viewer.origin

        ( dx, dy ) =
            ( pointer.movementX, pointer.movementY )
    in
        { viewer
            | origin =
                ( left - dx / viewer.zoom
                , top - dy / viewer.zoom
                )
        }



-- VIEW ##############################################################


{-| View the svg tag representing the DrawingArea model
-}
view : List (Svg.Attribute msg) -> AnnotationSet -> SvgViewer -> Svg msg
view attributes set viewer =
    Svg.svg
        (sizeStyleAttribute viewer :: attributes)
        [ Svg.g
            [ svgTransform viewer.zoom viewer.origin ]
            ((case viewer.bgImage of
                Nothing ->
                    []

                Just image ->
                    [ Image.viewSvg [] Nothing image ]
             )
                ++ if (optionValue "LastAnnotationOnly" viewer) then
                    AnnSet.viewLastSelection set
                   else
                    AnnSet.viewAllSelections set
            )
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
