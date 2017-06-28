port module Main exposing (..)

import Html exposing (..)
import Html.Attributes as HtmlA exposing (..)
import Html.Events exposing (..)
import Annotation.Geometry.Types exposing (BoundingBox)
import Annotation.Geometry.BoundingBox as BoundingBox
import Annotation.Geometry.Point as Point
import Annotation.Viewer as Viewer exposing (Viewer)
import Image exposing (Image)
import Mouse
import Svg
import Svg.Lazy
import Annotation.Svg as Svg
import Window


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
    , viewer : Viewer
    , currentTool : Tool
    , dragState : DragState
    }


type Tool
    = GrabMove
    | BoundingBox


type DragState
    = Up
    | DragMove
    | DragBBoxFrom Mouse.Coordinates


initialModel : Model
initialModel =
    { bgImage = Image "/img/300x200.png" 300 200
    , boundingBoxes = []
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
    model.boundingBoxes
        |> List.map (Svg.Lazy.lazy Svg.boundingBox)
        |> (::) (Image.viewSvg [] model.bgImage)
        |> Svg.g []
        |> Viewer.viewInWithDetails (id "viewer" :: mouseEvents) model.viewer


mouseEvents : List (Html.Attribute Msg)
mouseEvents =
    [ Mouse.onDown (MouseMsg << MouseDown)
    , Mouse.onMove (MouseMsg << MouseMove)
    , onMouseUp (MouseMsg MouseUp)
    ]



-- UPDATE ############################################################


type Msg
    = MouseMsg MouseMsg
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
        MouseMsg mouseMsg ->
            ( mouseUpdate mouseMsg model, Cmd.none )

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
            ( model, Cmd.none )

        WindowResizes ->
            ( model, askViewerSize () )

        ViewerSize size ->
            ( resizeViewer size model, Cmd.none )


resizeViewer : ( Float, Float ) -> Model -> Model
resizeViewer size model =
    { model | viewer = Viewer.setSize size model.viewer |> Viewer.fitImage 0.8 model.bgImage }


mouseUpdate : MouseMsg -> Model -> Model
mouseUpdate msg model =
    case ( msg, model.dragState, model.currentTool, model.boundingBoxes ) of
        ( MouseDown event, _, BoundingBox, bboxes ) ->
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

        ( MouseMove event, DragBBoxFrom startCoord, BoundingBox, _ :: bboxes ) ->
            let
                ( startPoint, point ) =
                    ( Point.fromCoordinates startCoord
                    , Point.fromCoordinates (Viewer.positionIn model.viewer event.offsetPos)
                    )

                bbox =
                    BoundingBox.fromPair ( startPoint, point )
            in
                { model | boundingBoxes = bbox :: bboxes }

        ( MouseDown event, _, GrabMove, _ ) ->
            { model | dragState = DragMove }

        ( MouseMove event, DragMove, GrabMove, _ ) ->
            { model | viewer = Viewer.grabMove event.movement model.viewer }

        ( MouseUp, _, _, _ ) ->
            { model | dragState = Up }

        _ ->
            model



-- SUBSCRIPTIONS #####################################################


port askViewerSize : () -> Cmd msg


port viewerSize : (( Float, Float ) -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Window.resizes (always WindowResizes)
        , viewerSize ViewerSize
        ]
