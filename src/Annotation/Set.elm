-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module Annotation.Set
    exposing
        ( Set
        , remove
        , view
        )

{-| Annotation.Set aims at managing a set of annotations.
Basically, it is an array of Annotation.
So anything that can be done on arrays can be done on an Annotation.Set.

# Model
@docs Set

# Update
@docs remove

# View
@docs view
-}

import Html exposing (Html)
import Array exposing (Array)
import Helpers.Array as Array
import Annotation exposing (Annotation)
import Svg exposing (Svg)
import Svg.Lazy exposing (lazy)
import Helpers.Select as Select


-- MODEL #############################################################


{-| An annotation set is basically an array of annotations.
-}
type alias Set =
    Array Annotation



-- UPDATE ############################################################


{-| Remove the annotation at the given index from the set.
-}
remove : Int -> Set -> Set
remove index set =
    Array.removeAt index set



-- VIEW ##############################################################


{-| View of the SVG representation of all the annotations in the set.
-}
view : Set -> List (Svg msg)
view set =
    Array.map (lazy Annotation.view) set
        |> Array.toList


type alias Option =
    Maybe ( Int, Annotation )


{-| Create a <select> tag with an <option> tag for each annotation.
-}
selectTag : (Option -> msg) -> Option -> Set -> Html msg
selectTag =
    Select.fromArray optionDescriber


optionDescriber : Option -> String
optionDescriber maybeOption =
    case maybeOption of
        Nothing ->
            ""

        Just ( id, annotation ) ->
            toString id
                ++ ": "
                ++ (case annotation.selection of
                        Ann.NoSelection ->
                            "No Selection"

                        Ann.RSel _ ->
                            "Rectangle"

                        Ann.OSel _ ->
                            "Outline"
                   )
                ++ " | "
                ++ annotation.label
