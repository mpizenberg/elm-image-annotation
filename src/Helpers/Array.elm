module Helpers.Array exposing (..)

import Array exposing (Array)


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
