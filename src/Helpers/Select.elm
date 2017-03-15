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
          -- Lists
        , listConfig
        , listTag
        )

import Html exposing (Html)
import Html.Attributes as Attributes
import Helpers.Events as Events
import Helpers.List as List
import Array.Hamt as Array exposing (Array)


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



-- AUTO LISTS ########################################################


{-| Automatically generates an encoder from a list of pairs and a default.
-}
listEncoder : List ( option, string ) -> string -> option -> string
listEncoder pairs default option =
    List.find (Tuple.first >> (==) option) pairs
        |> Maybe.withDefault ( option, default )
        |> Tuple.second


{-| Automatically generates a decoder from a list of pairs and a default.
-}
listDecoder : List ( option, string ) -> option -> string -> option
listDecoder pairs default value =
    List.find (Tuple.second >> (==) value) pairs
        |> Maybe.withDefault ( default, value )
        |> Tuple.first


listConfig : ( option, String ) -> List ( option, String ) -> Config option
listConfig ( defaultOption, defaultValue ) pairs =
    { describe = listEncoder pairs defaultValue
    , encode = listEncoder pairs defaultValue
    , decode = listDecoder pairs defaultOption
    , selected = (==)
    }


listTag : (option -> msg) -> option -> ( option, String ) -> List ( option, String ) -> Html msg
listTag tagger current default pairs =
    let
        options =
            Tuple.first default :: List.map Tuple.first pairs

        config =
            listConfig default pairs
    in
        tag config tagger current options



-- AUTO ARRAYS #######################################################


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
