module Helpers.Views exposing (..)

import Html as H exposing (Html)
import Html.Attributes as HA
import List.Extra as LE
import Array exposing (Array)
import String
import Helpers.Events as HPE


-- AUTO SELECT #######################################################


autoSelectTag : List ( value, String ) -> value -> (value -> msg) -> Html msg
autoSelectTag pairs =
    case pairs of
        [] ->
            always << always (H.select [] [])

        ( defaultValue, defaultString ) :: _ ->
            let
                encoder =
                    autoEncoder defaultString pairs

                config =
                    { describer = encoder
                    , encoder = encoder
                    , decoder = autoDecoder defaultValue pairs
                    , allValues = fst <| List.unzip pairs
                    }
            in
                selectTag config


selectTagFromArray :
    (( Int, value ) -> String)
    -> value
    -> Array value
    -> ( Int, value )
    -> (( Int, value ) -> msg)
    -> Html msg
selectTagFromArray describer defaultValue array =
    let
        config =
            { describer = describer
            , encoder = arrayEncoder
            , decoder = arrayDecoder defaultValue array
            , allValues = Array.toIndexedList array
            }
    in
        selectTag config



-- SELECT ENCODERS/DECODERS ##########################################


{-| Automatically generates an encoder from a list of pairs and a default.
-}
autoEncoder : string -> List ( value, string ) -> value -> string
autoEncoder default pairs value =
    LE.find (fst >> (==) value) pairs
        |> Maybe.withDefault ( value, default )
        |> snd


{-| Automatically generates a decoder from a list of pairs and a default.
-}
autoDecoder : value -> List ( value, string ) -> string -> value
autoDecoder default pairs string =
    LE.find (snd >> (==) string) pairs
        |> Maybe.withDefault ( default, string )
        |> fst


{-| An encoder for when dealing with elements of an array
-}
arrayEncoder : ( Int, value ) -> String
arrayEncoder ( id, value ) =
    toString id


{-| A decoder for when dealing with elements of an array
-}
arrayDecoder : value -> Array value -> String -> ( Int, value )
arrayDecoder defaultValue array stringId =
    case String.toInt stringId of
        Err _ ->
            ( -1, defaultValue )

        Ok id ->
            case Array.get id array of
                Nothing ->
                    ( -1, defaultValue )

                Just value ->
                    ( id, value )



-- MANUAL SELECT CONFIG ##############################################


type alias SelectConfig value =
    { describer : value -> String
    , encoder : value -> String
    , decoder : String -> value
    , allValues : List value
    }


selectTag : SelectConfig value -> value -> (value -> msg) -> Html msg
selectTag config currentValue msgMaker =
    H.select
        [ HPE.onChange <| msgMaker << config.decoder ]
        (List.map
            (optionTag config.encoder config.describer currentValue)
            config.allValues
        )


optionTag : (value -> String) -> (value -> String) -> value -> value -> Html msg
optionTag encoder describer currentValue value =
    H.option
        [ HA.value <| encoder value
        , HA.selected (currentValue == value)
        ]
        [ H.text <| describer value
        ]
