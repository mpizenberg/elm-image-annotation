-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module Main exposing (..)

import Html exposing (Html)
import Html.Events as Events
import Html.Attributes as Attributes
import Svg.Attributes as SvgAttributes
import Svg exposing (Svg)
import OpenSolid.Svg as Svg
import Array exposing (Array)
import Annotation exposing (Annotation)
import Annotation.Set as Set exposing (Set)
import DrawingArea.Viewer as Viewer exposing (Viewer)
import Tool exposing (Tool)
import Pointer exposing (Pointer)
import Image exposing (Image)
import OpenSolid.Geometry.Types exposing (Point2d(..), Vector2d(..), Circle2d(..))
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
    , annotation : Maybe Annotation
    , checked : Maybe Annotation.Check
    , pointerTrack : Pointer.Track
    }


bgImage : Image
bgImage =
    Image "http://lorempixel.com/200/200" 200 200


initModel : Model
initModel =
    { viewer = Viewer.fitImage 0.8 bgImage Viewer.default
    , bgImage = bgImage
    , annotation = Nothing
    , checked = Nothing
    , pointerTrack = Pointer.None
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )



-- UPDATE ############################################################


type Msg
    = PointerEventAnnotation Pointer


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PointerEventAnnotation pointer ->
            let
                annotation =
                    model.annotation
                        |> Maybe.map (\ann -> ( 0, ann ))
                        |> Annotation.update pointer model.pointerTrack Tool.Outline 0
                        |> Maybe.map Tuple.second

                checked =
                    case ( pointer.event, annotation ) of
                        ( Pointer.Up, Just ann ) ->
                            Just (Annotation.isValid ann)

                        ( Pointer.Down, _ ) ->
                            Nothing

                        _ ->
                            model.checked
            in
                ( { model
                    | annotation = annotation
                    , checked = checked
                    , pointerTrack = Pointer.updateTrack pointer model.pointerTrack
                  }
                , Cmd.none
                )



-- VIEW ##############################################################


toFeedback : Annotation.Check -> Html msg
toFeedback check =
    case check of
        Annotation.Valid ->
            Html.p [ Attributes.class "valid" ] [ Html.text "Valid annotation" ]

        Annotation.SegmentsCrossing point ->
            Html.p [ Attributes.class "invalid" ] [ Html.text ("Intersection at " ++ toString point) ]

        Annotation.AreaUnderLimit limit ->
            Html.p [ Attributes.class "invalid" ] [ Html.text ("Area under limit " ++ toString limit) ]

        _ ->
            Html.p [ Attributes.class "invalid" ] [ Html.text ("Invalid") ]


view : Model -> Html Msg
view model =
    let
        viewerContour =
            Attributes.style [ ( "border", "1px solid black" ) ]

        annotationOffsetOn eventName event =
            Pointer.offsetOn eventName event PointerEventAnnotation (Viewer.positionIn model.viewer)

        annotationTouchOffsetOn eventName event =
            Pointer.touchOffsetOn eventName event PointerEventAnnotation (Viewer.positionIn model.viewer)

        viewerEvents =
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

        visualFeedback =
            case model.checked of
                Just (Annotation.SegmentsCrossing point) ->
                    Circle2d { centerPoint = point, radius = 10 }
                        |> Svg.circle2d
                            [ SvgAttributes.stroke "blue"
                            , SvgAttributes.strokeWidth "3"
                            , SvgAttributes.fillOpacity "0"
                            ]

                _ ->
                    Svg.text "No feedback"

        viewer =
            model.annotation
                |> Maybe.map Annotation.view
                |> Maybe.withDefault (Svg.text "No annotation yet")
                |> flip (::) [ visualFeedback ]
                |> Svg.g []
                |> Viewer.innerView model.viewer (Just model.bgImage)
                |> Viewer.view (viewerContour :: viewerEvents) model.viewer

        feedbackText =
            model.checked
                |> Maybe.map toFeedback
                |> Maybe.withDefault (Html.div [ Attributes.hidden True ] [])
    in
        Html.div []
            [ viewer
            , feedbackText
            , Html.p [] [ Html.text <| toString model ]
            ]
