-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module Main exposing (..)

import Html exposing (Html)
import Html.Events as Events
import Html.Attributes as Attributes
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
    , annotations : Set
    , currentOption : Annotation.Option
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
    , annotations = Array.empty
    , currentOption = Nothing
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
                nbAnnotations =
                    Array.length model.annotations
            in
                ( { model | annotations = Set.remove (nbAnnotations - 1) model.annotations }
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

                currentOption =
                    case pointer.event of
                        Pointer.Up ->
                            Nothing

                        _ ->
                            newOption
            in
                ( { model
                    | currentOption = currentOption
                    , annotations = annotations
                    , pointerTrack = Pointer.updateTrack pointer model.pointerTrack
                  }
                , Cmd.none
                )



-- VIEW ##############################################################


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
            Viewer.viewSet (viewerContour :: viewerEvents) model.viewer (Just model.bgImage) model.annotations
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
