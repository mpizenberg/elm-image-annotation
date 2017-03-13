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
import Image exposing (Image)
import Json.Encode as Encode
import RLE exposing (RLE)


main =
    Html.program
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }



-- MODEL #############################################################


images =
    [ "/img/taj_mahal_01.jpg"
    , "/img/taj_mahal_07.jpg"
    , "/img/taj_mahal_10.jpg"
    ]


type alias Model =
    { viewer : Viewer
    , image : Maybe Image
    , groundtruth : Maybe RLE
    }


initModel : Model
initModel =
    { viewer = Viewer.default
    , image = Nothing
    , groundtruth = Nothing
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )



-- UPDATE ############################################################


type Msg
    = ChooseImage String
    | ImageFetched ( Image, RLE )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChooseImage url ->
            ( model, Cmd.none )

        ImageFetched ( image, rle ) ->
            ( model, Cmd.none )



-- VIEW ##############################################################


view : Model -> Html Msg
view model =
    let
        imageButton url =
            Html.button
                [ Events.onClick <| ChooseImage url ]
                [ Html.text url ]

        imagesButtons =
            List.map imageButton images

        viewerContour =
            Attributes.style [ ( "border", "1px solid black" ) ]

        visualGroundtruth =
            case model.groundtruth of
                Just rle ->
                    Svg.text "Groundtruth visible"

                Nothing ->
                    Svg.text "No groundtruth"

        viewer =
            visualGroundtruth
                |> Viewer.innerView model.viewer model.image
                |> Viewer.view [ viewerContour ] model.viewer
    in
        Html.div []
            [ Html.div [] imagesButtons
            , viewer
            , Html.p [] [ Html.text <| toString model ]
            ]
