-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module Main exposing (..)

import Html exposing (Html)
import Html.Events as Events
import Html.Attributes as Attributes
import Svg exposing (Svg)
import Svg.Lazy exposing (lazy)
import Array exposing (Array)
import Annotation exposing (Annotation)
import Annotation.Set as Set exposing (Set)
import DrawingArea.Viewer as Viewer exposing (Viewer)
import Tool exposing (Tool)
import Pointer exposing (Pointer)
import Image exposing (Image)
import OpenSolid.Geometry.Types exposing (Point2d(..), Vector2d(..))
import Json.Encode as Encode


main =
    Html.program
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }



-- MODEL #############################################################


type alias Model =
    { viewer : Viewer
    , bgImage : Image
    , visibleAnnotations : List ( Int, Annotation )
    , deletedAnnotations : List ( Int, Annotation )
    , nextId : Int
    , currentTool : Tool
    , pointerTrack : Pointer.Track
    , jsonExport : String
    }


bgImage : Image
bgImage =
    Image "http://lorempixel.com/200/200" 200 200


initModel : Model
initModel =
    { viewer = Viewer.fitImage 0.8 bgImage Viewer.default
    , bgImage = bgImage
    , visibleAnnotations = []
    , deletedAnnotations = []
    , nextId = 0
    , currentTool = Tool.None
    , pointerTrack = Pointer.None
    , jsonExport = ""
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )



-- UPDATE ############################################################


type Msg
    = DeleteLast
    | ToolFG
    | ToolBG
    | PointerEventAnnotation Pointer


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DeleteLast ->
            let
                head =
                    List.head model.visibleAnnotations

                tail =
                    List.tail model.visibleAnnotations
                        |> Maybe.withDefault []

                deletedAnnotations =
                    case head of
                        Nothing ->
                            model.deletedAnnotations

                        Just del ->
                            del :: model.deletedAnnotations
            in
                ( { model
                    | visibleAnnotations = tail
                    , deletedAnnotations = deletedAnnotations
                  }
                , Cmd.none
                )

        ToolFG ->
            ( { model | currentTool = Tool.ScribbleFG }
            , Cmd.none
            )

        ToolBG ->
            ( { model | currentTool = Tool.ScribbleBG }
            , Cmd.none
            )

        PointerEventAnnotation pointer ->
            let
                headVisible =
                    List.head model.visibleAnnotations

                headDeleted =
                    List.head model.deletedAnnotations

                tailVisible =
                    List.tail model.visibleAnnotations
                        |> Maybe.withDefault []

                newHeadOption =
                    case pointer.event of
                        Pointer.Down ->
                            Annotation.update pointer model.pointerTrack model.currentTool model.nextId Nothing

                        _ ->
                            Annotation.update pointer model.pointerTrack model.currentTool model.nextId headVisible

                visibleAnnotations =
                    case ( pointer.event, newHeadOption ) of
                        ( Pointer.Down, Just ann ) ->
                            ann :: model.visibleAnnotations

                        ( _, Just ann ) ->
                            ann :: tailVisible

                        _ ->
                            model.visibleAnnotations

                nextId =
                    case pointer.event of
                        Pointer.Down ->
                            model.nextId + 1

                        _ ->
                            model.nextId
            in
                ( { model
                    | visibleAnnotations = visibleAnnotations
                    , nextId = nextId
                    , pointerTrack = Pointer.updateTrack pointer model.pointerTrack
                  }
                , Cmd.none
                )



-- VIEW ##############################################################


viewScribbles : List ( Int, Annotation ) -> List (Svg Msg)
viewScribbles scribbles =
    scribbles
        |> List.map (lazy (Tuple.second >> Annotation.view))


view : Model -> Html Msg
view model =
    let
        button msg text =
            Html.button [ Events.onClick msg ] [ Html.text text ]

        viewerContour =
            Attributes.style [ ( "border", "1px solid black" ) ]

        annotationOffsetOn eventName event =
            Pointer.offsetOn eventName event PointerEventAnnotation (Viewer.positionIn model.viewer)

        annotationTouchOffsetOn eventName event =
            Pointer.touchOffsetOn eventName event PointerEventAnnotation (Viewer.positionIn model.viewer)

        viewerEvents =
            case model.currentTool of
                Tool.None ->
                    []

                _ ->
                    [ annotationOffsetOn "mousedown" Pointer.Down
                    , annotationTouchOffsetOn "touchstart" Pointer.Down
                    , annotationTouchOffsetOn "touchmove" Pointer.Move
                    , annotationOffsetOn "mouseup" Pointer.Up
                    , annotationTouchOffsetOn "touchend" Pointer.Up
                    ]
                        ++ if model.pointerTrack == Pointer.None then
                            []
                           else
                            [ annotationOffsetOn "mousemove" Pointer.Move ]

        viewer =
            viewScribbles model.visibleAnnotations
                |> Svg.g []
                |> Viewer.innerView model.viewer (Just model.bgImage)
                |> Viewer.view (viewerContour :: viewerEvents) model.viewer
    in
        Html.div []
            [ Html.p []
                [ button DeleteLast "Delete Last"
                , button ToolFG "FG Scribble"
                , button ToolBG "BG Scribble"
                ]
            , viewer
            , Html.p [] [ Html.text <| toString model ]
            ]
