-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module AnnotationSet exposing (..)

{-| AnnotationSet aims at managing a set of annotations.
-}

import String
import Html as H exposing (Html)
import Html.Attributes as HA
import Svg exposing (Svg)
import Json.Encode as JE
import Array exposing (Array)
import Annotation as Ann exposing (Annotation)
import Helpers.Events as HPE


-- MODEL #############################################################


type alias AnnotationSet =
    Array Annotation



-- VIEW ##############################################################


viewAllSelections : AnnotationSet -> List (Svg msg)
viewAllSelections set =
    Array.map Ann.selectionView set
        |> Array.toList


viewLastSelection : AnnotationSet -> List (Svg msg)
viewLastSelection set =
    let
        length =
            Array.length set
    in
        viewAllSelections <| Array.slice (length - 1) length set


{-| Create a <select> tag with an <option> tag for each annotation
   currentId is the id of the currently selected option.
-}
selectTag : Maybe Int -> (Maybe Int -> msg) -> AnnotationSet -> Html msg
selectTag currentId msgMaker set =
    H.select
        [ HPE.onChange <| msgMaker << Result.toMaybe << String.toInt ]
        (H.option
            [ HA.value "none", HA.selected (currentId == Nothing) ]
            [ H.text "None" ]
            :: List.map
                (Ann.optionTag currentId)
                (Array.toIndexedList set)
        )



-- OUTPUTS ##############################################################


object : AnnotationSet -> JE.Value
object set =
    JE.array <| Array.map Ann.object set


pathsObject : AnnotationSet -> JE.Value
pathsObject set =
    JE.array <| Array.map Ann.pathObject set



-- OTHER #############################################################


{-| Indicates if the annotation set has at least one selection
-}
hasSelection : AnnotationSet -> Bool
hasSelection set =
    List.any Ann.hasSelection <| Array.toList set
