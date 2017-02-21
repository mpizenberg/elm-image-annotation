-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module Main exposing (..)

import Html exposing (Html)
import Html.Events as Events
import Helpers.Events as Events
import Html.Attributes as Attributes
import Array exposing (Array)
import Annotation exposing (Annotation)
import Annotation.Set as Set exposing (Set)
import DrawingArea.Viewer as Viewer exposing (Viewer)
import Tool exposing (Tool)
import Pointer exposing (Pointer)
import Image exposing (Image)
import OpenSolid.Geometry.Types exposing (Point2d(..), Vector2d(..))


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
    , annotations : Set
    , currentOption : Annotation.Option
    , currentTool : Tool
    , pointerTrack : Pointer.Track
    }


bgImage : Image
bgImage =
    Image "http://lorempixel.com/200/200" 200 200


initModel : Model
initModel =
    { viewer = Viewer.fitImage 0.8 bgImage Viewer.default
    , bgImage = bgImage
    , annotations = Array.empty
    , currentOption = Nothing
    , currentTool = Tool.None
    , pointerTrack = Pointer.None
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )



-- UPDATE ############################################################


type Msg
    = AnnotationSelected Annotation.Option
    | DeleteAnnotation Annotation.Option
    | ToolSelected Tool
    | PointerEventViewer Pointer
    | PointerEventAnnotation Pointer


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AnnotationSelected option ->
            ( { model | currentOption = option }
            , Cmd.none
            )

        DeleteAnnotation option ->
            case option of
                Nothing ->
                    ( model, Cmd.none )

                Just ( id, _ ) ->
                    ( { model | annotations = Set.remove id model.annotations }
                    , Cmd.none
                    )

        ToolSelected tool ->
            ( { model | currentTool = tool }
            , Cmd.none
            )

        PointerEventViewer pointer ->
            let
                newViewer =
                    case pointer.event of
                        Pointer.Move ->
                            model.viewer
                                |> Viewer.move (Vector2d <| Pointer.movement pointer)

                        _ ->
                            model.viewer
            in
                ( { model
                    | viewer = newViewer
                    , pointerTrack = Pointer.updateTrack pointer model.pointerTrack
                  }
                , Cmd.none
                )

        PointerEventAnnotation pointer ->
            let
                nbAnnotations =
                    Array.length model.annotations

                newOption =
                    Annotation.update pointer model.pointerTrack model.currentTool nbAnnotations model.currentOption

                annotations =
                    case newOption of
                        Nothing ->
                            model.annotations

                        Just ( id, annotation ) ->
                            if id == nbAnnotations then
                                Array.push annotation model.annotations
                            else
                                Array.set id annotation model.annotations
            in
                ( { model
                    | currentOption = newOption
                    , annotations = annotations
                    , pointerTrack = Pointer.updateTrack pointer model.pointerTrack
                  }
                , Cmd.none
                )



-- VIEW ##############################################################


view : Model -> Html Msg
view model =
    let
        selectAnnotationTag =
            Set.selectTag AnnotationSelected model.currentOption model.annotations

        deleteAnnotationButton =
            Html.button
                [ Events.onClick <| DeleteAnnotation model.currentOption ]
                [ Html.text "Delete" ]

        viewerContour =
            Attributes.style [ ( "border", "1px solid black" ) ]

        viewerMovementOn eventName event =
            Pointer.movementOn eventName event PointerEventViewer identity

        annotationOffsetOn eventName event =
            Pointer.offsetOn eventName event PointerEventAnnotation (Viewer.positionIn model.viewer)

        viewerEvents =
            case model.currentTool of
                Tool.None ->
                    [ viewerMovementOn "mousedown" Pointer.Down
                    , viewerMovementOn "mouseup" Pointer.Up
                    ]
                        ++ if model.pointerTrack == Pointer.None then
                            []
                           else
                            [ viewerMovementOn "mousemove" Pointer.Move ]

                _ ->
                    [ annotationOffsetOn "mousedown" Pointer.Down
                    , annotationOffsetOn "mouseup" Pointer.Up
                    ]
                        ++ if model.pointerTrack == Pointer.None then
                            []
                           else
                            [ annotationOffsetOn "mousemove" Pointer.Move ]

        viewer =
            Viewer.viewSet (viewerContour :: viewerEvents) model.viewer (Just model.bgImage) model.annotations
    in
        Html.div []
            [ Html.p []
                [ Html.text "Current annotation: "
                , selectAnnotationTag
                , deleteAnnotationButton
                ]
            , Html.p []
                [ Html.text "Current tool: "
                , Tool.selectTag ToolSelected model.currentTool
                ]
            , viewer
            , Html.p [] [ Html.text <| toString model ]
            ]
