-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


port module Main exposing (..)

import Html exposing (Html)
import Html.Events as Events
import Html.Attributes as Attributes
import Svg exposing (Svg)
import Svg.Attributes as SvgA
import DrawingArea.Viewer as Viewer exposing (Viewer)
import Image exposing (Image)
import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder)
import RLE exposing (RLE)
import Matrix exposing (Matrix)


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
    | GroundtruthFetched ( String, String )


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
                ( { model
                    | image = Just image
                    , groundtruth = Nothing
                    , viewer = viewer
                  }
                , fetchGroundtruth name
                )

        GroundtruthFetched ( name, rleString ) ->
            let
                groundtruth : Maybe RLE
                groundtruth =
                    Decode.decodeString RLE.decode rleString
                        |> Result.toMaybe

                groundtruthMatrixValue : Maybe Encode.Value
                groundtruthMatrixValue =
                    groundtruth
                        |> Maybe.map RLE.toMatrix
                        |> Maybe.map RLE.encodeMatrix

                displayCmd =
                    case groundtruthMatrixValue of
                        Just value ->
                            displayGroundtruth ( "groundtruth", value )

                        _ ->
                            Cmd.none
            in
                ( { model | groundtruth = groundtruth }
                , displayCmd
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
                    Svg.image
                        [ Attributes.id "groundtruth"
                        , SvgA.x "0"
                        , SvgA.y "0"
                        , SvgA.width <| toString rle.width
                        , SvgA.height <| toString rle.height
                        ]
                        [ Svg.text "Groundtruth visible" ]

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
              --, Html.p [] [ Html.text <| toString model ]
            ]



-- PORTS #############################################################


port chooseImage : String -> Cmd msg


port imageFetched : (( String, String, ( Int, Int ) ) -> msg) -> Sub msg


port fetchGroundtruth : String -> Cmd msg


port groundtruthFetched : (( String, String ) -> msg) -> Sub msg


port displayGroundtruth : ( String, Encode.Value ) -> Cmd msg



-- SUBS #############################################################


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ imageFetched ImageFetched
        , groundtruthFetched GroundtruthFetched
        ]
