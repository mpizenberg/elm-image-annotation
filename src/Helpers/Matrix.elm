-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module Helpers.Matrix exposing (..)

import Matrix exposing (Matrix)
import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder)
import Array.Hamt as Array exposing (Array)
import Helpers.Array as Array


encode : Matrix Bool -> Encode.Value
encode { size, data } =
    Encode.object
        [ ( "width", Tuple.first size |> Encode.int )
        , ( "height", Tuple.second size |> Encode.int )
        , ( "data", data |> Array.map Encode.bool |> Array.encode )
        ]


decode : Decoder (Matrix Bool)
decode =
    Decode.map2 Matrix
        (Decode.map2 (,)
            (Decode.field "width" Decode.int)
            (Decode.field "height" Decode.int)
        )
        (Decode.field "data" <| Array.decode Decode.bool)
