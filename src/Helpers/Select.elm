-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module Helpers.Select
    exposing
        ( Config
        , tag
          -- Arrays
        , ArrayOption
        , arrayConfig
        , arrayTag
        )

import Html exposing (Html)
import Html.Attributes as Attributes
import Helpers.Events as Events
import Helpers.List as List
import Array exposing (Array)


type alias Config option =
    { describe : option -> String
    , encode : option -> String
    , decode : String -> option
    , selected : option -> option -> Bool
    }


tag : Config option -> (option -> msg) -> option -> List option -> Html msg
tag config tagger current options =
    options
        |> List.map (optionTag config current)
        |> Html.select [ Events.onChange (tagger << config.decode) ]


optionTag : Config option -> option -> option -> Html msg
optionTag config current option =
    Html.option
        [ Attributes.value (config.encode option)
        , Attributes.selected (config.selected current option)
        ]
        [ Html.text (config.describe option)
        ]



-- AUTO CODECS #######################################################


{-| Automatically generates an encoder from a list of pairs and a default.
-}
autoEncoder : string -> option -> List ( option, string ) -> string
autoEncoder default option pairs =
    List.find (Tuple.first >> (==) option) pairs
        |> Maybe.withDefault ( option, default )
        |> Tuple.second


{-| Automatically generates a decoder from a list of pairs and a default.
-}
autoDecoder : string -> List ( option, string ) -> Maybe option
autoDecoder value pairs =
    List.find (Tuple.second >> (==) value) pairs
        |> Maybe.map Tuple.first



-- ARRAYS ############################################################


type alias ArrayOption value =
    Maybe ( Int, value )


{-| Automatic encoder for array elements.
-}
arrayEncoder : ArrayOption value -> String
arrayEncoder option =
    case option of
        Nothing ->
            toString -1

        Just ( id, _ ) ->
            toString id


{-| Automatic decoder for array elements.
-}
arrayDecoder : Array value -> String -> ArrayOption value
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


arraySelected : ArrayOption value -> ArrayOption value -> Bool
arraySelected maybeCurrent maybeOption =
    case ( maybeCurrent, maybeOption ) of
        ( Nothing, Nothing ) ->
            True

        ( Just ( currentId, _ ), Just ( optionId, _ ) ) ->
            currentId == optionId

        _ ->
            False


arrayConfig : (ArrayOption value -> String) -> Array value -> Config (ArrayOption value)
arrayConfig describe array =
    { describe = describe
    , encode = arrayEncoder
    , decode = arrayDecoder array
    , selected = arraySelected
    }


arrayTag :
    (ArrayOption value -> String)
    -> (ArrayOption value -> msg)
    -> ArrayOption value
    -> Array value
    -> Html msg
arrayTag describe tagger current array =
    let
        options =
            Array.toIndexedList array
                |> List.map Just
                |> (::) Nothing

        config =
            arrayConfig describe array
    in
        options
            |> List.map (optionTag config current)
            |> Html.select [ Events.onChange (tagger << config.decode) ]
