-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


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


concat : Array (Array a) -> Array a
concat =
    Array.foldr Array.append Array.empty
