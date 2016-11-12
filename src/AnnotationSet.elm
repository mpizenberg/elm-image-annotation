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
import Helpers.Views as HPV
import Helpers.Array as HPA
import Tools exposing (Tool)


-- MODEL #############################################################


type alias AnnotationSet =
    Array Annotation



-- UPDATE ############################################################


remove : Int -> AnnotationSet -> AnnotationSet
remove id set =
    HPA.removeAt id set


update : Ann.Event -> ( Int, Int ) -> ( Int, Int ) -> Tool -> Maybe ( Int, Annotation ) -> AnnotationSet -> ( Maybe ( Int, Annotation ), AnnotationSet )
update event origin newPos tool current set =
    case current of
        Nothing ->
            ( Nothing, set )

        Just ( id, annotation ) ->
            let
                newAnnotation =
                    Ann.update event origin newPos tool annotation
            in
                ( Just ( id, newAnnotation )
                , Array.set id newAnnotation set
                )



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


{-| Create a <select> tag with an <option> tag for each annotation.
currentId is the id of the currently selected option.

type Msg
    = SelectAnnotation (Int, Annotation)

currentAnnotation =
    Maybe.withDefault Ann.default <| Array.get currentId set

html =
    H.div
        []
        (selectTag set (currentId, currentAnnotation) SelectAnnotation)
-}
selectTag :
    AnnotationSet
    -> Maybe ( Int, Annotation )
    -> (Maybe ( Int, Annotation ) -> msg)
    -> Html msg
selectTag =
    HPV.selectTagFromArray optionDescriber


optionDescriber : Maybe ( Int, Annotation ) -> String
optionDescriber maybeItem =
    case maybeItem of
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
