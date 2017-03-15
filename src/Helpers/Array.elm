-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module Helpers.Array exposing (..)

import Array.Hamt as Array exposing (Array)
import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder)


{-| Remove the element at the given index.
-}
removeAt : Int -> Array a -> Array a
removeAt id array =
    let
        length =
            Array.length array
    in
        if (length > 0 && id >= 0 && id < length) then
            Array.append
                (Array.slice 0 id array)
                (Array.slice (id + 1) length array)
        else
            array


concat : Array (Array a) -> Array a
concat =
    Array.foldr Array.append Array.empty


decode : Decoder a -> Decoder (Array a)
decode =
    Decode.list >> Decode.map Array.fromList


encode : Array Encode.Value -> Encode.Value
encode array =
    Array.toList array
        |> Encode.list



-- EXTRA


apply : Array (a -> b) -> Array a -> Array b
apply fs xs =
    let
        l =
            min (Array.length fs) (Array.length xs)

        fs_ =
            Array.slice 0 l fs
    in
        Array.indexedMap (\n f -> f (getUnsafe n xs)) fs_


map2 : (a -> b -> result) -> Array a -> Array b -> Array result
map2 f ws =
    apply (Array.map f ws)


getUnsafe : Int -> Array a -> a
getUnsafe n xs =
    case Array.get n xs of
        Just x ->
            x

        Nothing ->
            Debug.crash ("Index " ++ toString n ++ " of Array with length " ++ toString (Array.length xs) ++ " is not reachable.")
