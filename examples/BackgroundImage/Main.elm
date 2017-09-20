port module Main exposing (..)

import Html exposing (..)
import Html.Attributes as HtmlA exposing (..)
import Html.Events exposing (..)
import Annotation.Geometry.Types exposing (BoundingBox, Contour, Point)
import Annotation.Geometry.BoundingBox as BoundingBox
import Annotation.Geometry.Stroke as Stroke
import Annotation.Geometry.Contour as Contour
import Annotation.Geometry.Point as Point
import Annotation.Viewer as Viewer exposing (Viewer)
import Image exposing (Image)
import Mouse
import Svg exposing (Svg)
import Annotation.Svg as Svg
import Window
import Json.Encode as Encode
import Json.Decode as Decode
import OpenSolid.Geometry.Encode as Encode
import Keyboard


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL #############################################################


type alias Model =
    { bgImage : Image
    , boundingBoxes : List BoundingBox
    , contours : List Contour
    , tempContour : List Point
    , viewer : Viewer
    , currentTool : Tool
    , dragState : DragState
    }


type Tool
    = GrabMove
    | BoundingBox
    | Contour


type DragState
    = Up
    | DragMove
    | DragBBoxFrom Mouse.Coordinates
    | DragContour


initialModel : Model
initialModel =
    { bgImage = Image "/public/img/droppable.svg" 300 200
    , boundingBoxes = []
    , contours = []
    , tempContour = []
    , viewer = Viewer.default
    , currentTool = GrabMove
    , dragState = Up
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, askViewerSize () )



-- VIEW ##############################################################


view : Model -> Html Msg
view model =
    div [ id "annotation-app" ] [ toolbar model.currentTool, viewer model ]


toolbar : Tool -> Html Msg
toolbar currentTool =
    div [ id "toolbar" ]
        [ toolButton "tool-grab-move" GrabMove currentTool
        , toolButton "tool-bbox" BoundingBox currentTool
        , toolButton "tool-polygon" Contour currentTool
        , controlButton "control-zoom-in" ZoomIn
        , controlButton "control-zoom-out" ZoomOut
        , textButton "reset" Reset
        , textButton "export annotations" ExportAnnotations
        ]


toolButton : String -> Tool -> Tool -> Html Msg
toolButton cssId tool selectedTool =
    if (tool == selectedTool) then
        span [ id cssId, class "btn-tool selected", onClick (Select tool) ] []
    else
        span [ id cssId, class "btn-tool", onClick (Select tool) ] []


controlButton : String -> Msg -> Html Msg
controlButton cssId msg =
    span [ id cssId, class "btn-tool", onClick msg ] []


textButton : String -> Msg -> Html Msg
textButton displayText msg =
    span [ class "text-tool", onClick msg ] [ text displayText ]


viewer : Model -> Html Msg
viewer model =
    let
        svgBBoxes =
            model.boundingBoxes
                |> List.map Svg.boundingBox
                |> Svg.g []

        svgContours =
            model.contours
                |> List.map Svg.contour
                |> Svg.g []

        svgTempContour =
            Stroke.fromPoints model.tempContour
                |> Svg.stroke

        bgImage =
            Image.viewSvg [] model.bgImage
    in
        [ bgImage, svgBBoxes, svgContours, svgTempContour ]
            |> Svg.g []
            |> Viewer.viewInWithDetails (id "viewer" :: mouseEvents ++ dropEvents) model.viewer


mouseEvents : List (Attribute Msg)
mouseEvents =
    [ Mouse.onDown (MouseMsg << MouseDown)
    , Mouse.onMove (MouseMsg << MouseMove)
    , onMouseUp (MouseMsg MouseUp)
    ]


dropEvents : List (Attribute Msg)
dropEvents =
    [ onWithOptions "dragover" stopAndPrevent (Decode.succeed DoNothing)
    , Decode.at [ "dataTransfer", "files", "0" ] Decode.value
        |> Decode.map LoadImageFile
        |> onWithOptions "drop" stopAndPrevent
    ]


stopAndPrevent : Html.Events.Options
stopAndPrevent =
    { stopPropagation = True
    , preventDefault = True
    }



-- UPDATE ############################################################


type Msg
    = DoNothing
    | LoadImageFile Encode.Value
    | ImageLoaded ( String, Int, Int )
    | MouseMsg MouseMsg
    | KeyUp Keyboard.KeyCode
    | Select Tool
    | ZoomIn
    | ZoomOut
    | Reset
    | ExportAnnotations
    | WindowResizes
    | ViewerSize ( Float, Float )


type MouseMsg
    = MouseDown Mouse.Event
    | MouseMove Mouse.Event
    | MouseUp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DoNothing ->
            ( model, Cmd.none )

        LoadImageFile jsValue ->
            ( model, loadImageFile jsValue )

        ImageLoaded ( src, width, height ) ->
            ( resetImage (Image src width height) model, Cmd.none )

        MouseMsg mouseMsg ->
            ( mouseUpdate mouseMsg model, Cmd.none )

        KeyUp keycode ->
            if (keycode == 13 && model.currentTool == Contour && not (List.isEmpty model.tempContour)) then
                ( closeTempContour model, Cmd.none )
            else
                ( model, Cmd.none )

        Select tool ->
            ( { model | currentTool = tool }, Cmd.none )

        ZoomIn ->
            ( { model | viewer = Viewer.zoomIn model.viewer }
            , Cmd.none
            )

        ZoomOut ->
            ( { model | viewer = Viewer.zoomOut model.viewer }
            , Cmd.none
            )

        Reset ->
            ( resizeViewer model.viewer.size initialModel, Cmd.none )

        ExportAnnotations ->
            ( model, exportAnnotations <| serializeAnnotations model )

        WindowResizes ->
            ( model, askViewerSize () )

        ViewerSize size ->
            ( resizeViewer size model, Cmd.none )


closeTempContour : Model -> Model
closeTempContour model =
    { model
        | tempContour = []
        , contours = Contour.fromPoints model.tempContour :: model.contours
    }


resetImage : Image -> Model -> Model
resetImage image model =
    { initialModel | bgImage = image }
        |> resizeViewer model.viewer.size


resizeViewer : ( Float, Float ) -> Model -> Model
resizeViewer size model =
    { model | viewer = Viewer.setSize size model.viewer |> Viewer.fitImage 0.8 model.bgImage }


serializeAnnotations : Model -> String
serializeAnnotations model =
    let
        bboxes =
            List.map Encode.boundingBox2d model.boundingBoxes
                |> Encode.list

        contours =
            List.map Encode.polygon2d model.contours
                |> Encode.list
    in
        Encode.object
            [ ( "boundingBoxes", bboxes )
            , ( "contours", contours )
            ]
            |> Encode.encode 0


mouseUpdate : MouseMsg -> Model -> Model
mouseUpdate msg model =
    case ( msg, model.dragState, model.currentTool, model.boundingBoxes, model.tempContour ) of
        -- BoundingBox
        ( MouseDown event, _, BoundingBox, bboxes, _ ) ->
            let
                position =
                    Viewer.positionIn model.viewer event.offsetPos

                point =
                    Point.fromCoordinates position

                bbox =
                    BoundingBox.fromPair ( point, point )
            in
                { model
                    | dragState = DragBBoxFrom position
                    , boundingBoxes = bbox :: bboxes
                }

        ( MouseMove event, DragBBoxFrom startCoord, BoundingBox, _ :: bboxes, _ ) ->
            let
                ( startPoint, point ) =
                    ( Point.fromCoordinates startCoord
                    , Point.fromCoordinates (Viewer.positionIn model.viewer event.offsetPos)
                    )

                bbox =
                    BoundingBox.fromPair ( startPoint, point )
            in
                { model | boundingBoxes = bbox :: bboxes }

        -- GrabMove
        ( MouseDown event, _, GrabMove, _, _ ) ->
            { model | dragState = DragMove }

        ( MouseMove event, DragMove, GrabMove, _, _ ) ->
            { model | viewer = Viewer.grabMove event.movement model.viewer }

        -- Contour
        ( MouseDown event, _, Contour, _, stroke ) ->
            let
                position =
                    Viewer.positionIn model.viewer event.offsetPos

                point =
                    Point.fromCoordinates position
            in
                { model | tempContour = point :: stroke, dragState = DragContour }

        ( MouseMove event, DragContour, Contour, _, oldPoint :: stroke ) ->
            let
                position =
                    Viewer.positionIn model.viewer event.offsetPos

                point =
                    Point.fromCoordinates position
            in
                { model | tempContour = point :: stroke }

        -- Up
        ( MouseUp, _, _, _, _ ) ->
            { model | dragState = Up }

        _ ->
            model



-- SUBSCRIPTIONS #####################################################


port loadImageFile : Encode.Value -> Cmd msg


port imageLoaded : (( String, Int, Int ) -> msg) -> Sub msg


port exportAnnotations : String -> Cmd msg


port askViewerSize : () -> Cmd msg


port viewerSize : (( Float, Float ) -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Window.resizes (always WindowResizes)
        , imageLoaded ImageLoaded
        , viewerSize ViewerSize
        , Keyboard.ups KeyUp
        ]
