-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


port module Main exposing (..)

import Html exposing (Html)
import Html.Events as Events
import Html.Attributes as Attributes
import Svg exposing (Svg)
import DrawingArea.Viewer as Viewer exposing (Viewer)
import Image exposing (Image)
import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder)
import RLE exposing (RLE)


main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL #############################################################


images =
    [ "/img/taj_mahal_01"
    , "/img/taj_mahal_07"
    , "/img/taj_mahal_10"
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
    | ImageFetched ( String, String, ( Int, Int ) )
    | GroundtruthFetched Encode.Value


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChooseImage name ->
            ( model, chooseImage name )

        ImageFetched ( name, url, ( width, height ) ) ->
            let
                image =
                    Image url width height

                viewer =
                    Viewer.fitImage 0.8 image model.viewer
            in
                ( { model | image = Just image, viewer = viewer }
                , fetchGroundtruth name
                )

        GroundtruthFetched rleValue ->
            let
                groundtruth =
                    Decode.decodeValue RLE.decode rleValue
                        |> Result.toMaybe

                _ =
                    Debug.log "groundtruth" rleValue
            in
                ( { model | groundtruth = groundtruth }
                , Cmd.none
                )



-- VIEW ##############################################################


view : Model -> Html Msg
view model =
    let
        imageButton name =
            Html.button
                [ Events.onClick <| ChooseImage name ]
                [ Html.text name ]

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



-- PORTS #############################################################


port chooseImage : String -> Cmd msg


port imageFetched : (( String, String, ( Int, Int ) ) -> msg) -> Sub msg


port fetchGroundtruth : String -> Cmd msg


port groundtruthFetched : (Encode.Value -> msg) -> Sub msg



-- SUBS #############################################################


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ imageFetched ImageFetched
        , groundtruthFetched GroundtruthFetched
        ]
