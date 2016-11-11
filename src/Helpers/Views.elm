module Helpers.Views exposing (..)

import Html as H exposing (Html)
import Html.Attributes as HA
import Array exposing (Array)
import String
import Helpers.Events as HPE
import Helpers.List as HPL


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
                    , compare = (==)
                    }
            in
                selectTag config


selectTagFromArray :
    -- describer
    (Maybe ( Int, value ) -> String)
    -- array of values
    -> Array value
       -- currentValue
    -> Maybe ( Int, value )
       -- msgMaker
    -> (Maybe ( Int, value ) -> msg)
       -- <select> tag
    -> Html msg
selectTagFromArray describer array =
    let
        compare : Maybe ( Int, value ) -> Maybe ( Int, value ) -> Bool
        compare maybe1 maybe2 =
            case ( maybe1, maybe2 ) of
                ( Nothing, Nothing ) ->
                    True

                ( Just ( id1, _ ), Just ( id2, _ ) ) ->
                    id1 == id2

                _ ->
                    False

        -- allValues : List (Maybe ( Int, value ))
        allValues =
            Nothing :: (List.map Just <| Array.toIndexedList array)

        config =
            { describer = describer
            , encoder = arrayEncoder
            , decoder = arrayDecoder array
            , allValues = allValues
            , compare = compare
            }
    in
        selectTag config



-- SELECT ENCODERS/DECODERS ##########################################


{-| Automatically generates an encoder from a list of pairs and a default.
-}
autoEncoder : string -> List ( value, string ) -> value -> string
autoEncoder default pairs value =
    HPL.find (fst >> (==) value) pairs
        |> Maybe.withDefault ( value, default )
        |> snd


{-| Automatically generates a decoder from a list of pairs and a default.
-}
autoDecoder : value -> List ( value, string ) -> string -> value
autoDecoder default pairs string =
    HPL.find (snd >> (==) string) pairs
        |> Maybe.withDefault ( default, string )
        |> fst


{-| An encoder for when dealing with elements of an array
-}
arrayEncoder : Maybe ( Int, value ) -> String
arrayEncoder maybeItem =
    case maybeItem of
        Nothing ->
            "-1"

        Just ( id, value ) ->
            toString id


{-| A decoder for when dealing with elements of an array
-}
arrayDecoder : Array value -> String -> Maybe ( Int, value )
arrayDecoder array stringId =
    case String.toInt stringId of
        Err _ ->
            Nothing

        Ok id ->
            case Array.get id array of
                Nothing ->
                    Nothing

                Just value ->
                    Just ( id, value )



-- MANUAL SELECT CONFIG ##############################################


type alias SelectConfig value =
    { describer : value -> String
    , encoder : value -> String
    , decoder : String -> value
    , allValues : List value
    , compare : value -> value -> Bool
    }


selectTag : SelectConfig value -> value -> (value -> msg) -> Html msg
selectTag config currentValue msgMaker =
    H.select
        [ HPE.onChange <| msgMaker << config.decoder ]
        (List.map
            (optionTag config.describer config.encoder config.compare currentValue)
            config.allValues
        )


optionTag :
    -- describer
    (value -> String)
    -- encoder
    -> (value -> String)
       -- compareFunction
    -> (value -> value -> Bool)
       -- currentValue
    -> value
       -- option value
    -> value
       -- <option> tag
    -> Html msg
optionTag describer encoder compareFunction currentValue value =
    H.option
        [ HA.value <| encoder value
        , HA.selected <| compareFunction currentValue value
        ]
        [ H.text <| describer value
        ]
