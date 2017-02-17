-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module Helpers.Select exposing (..)

import Html exposing (Html)
import Html.Attributes as Attributes
import Helpers.Events as Events
import Helpers.List as List
import Array exposing (Array)


type alias Option key value =
    ( key, value )


type alias Config option =
    { describe : option -> String
    , encode : option -> String
    , decode : String -> Maybe option
    , compare : Maybe option -> option -> Bool
    }


tag : Config option -> (Maybe option -> msg) -> Maybe option -> List option -> Html msg
tag config tagger current options =
    options
        |> List.map (optionTag config current)
        |> Html.select [ Events.onChange (tagger << config.decode) ]


optionTag : Config option -> Maybe option -> option -> Html msg
optionTag config current option =
    Html.option
        [ Attributes.value (config.encode option)
        , Attributes.selected (config.compare current option)
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


{-| Automatic encoder for array elements.
-}
arrayEncoder : Option Int value -> String
arrayEncoder ( id, value ) =
    toString id


{-| Automatic decoder for array elements.
-}
arrayDecoder : String -> Array value -> Maybe (Option Int value)
arrayDecoder stringId array =
    case String.toInt stringId of
        Err _ ->
            Nothing

        Ok id ->
            case Array.get id array of
                Nothing ->
                    Nothing

                Just value ->
                    Just ( id, value )
