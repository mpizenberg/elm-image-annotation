-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module AnnotationSet
    exposing
        ( AnnotationSet
          -- UPDATE
        , remove
          -- VIEW
        , viewAllSelections
        , selectTag
          -- OUTPUT
        , object
        , pathsObject
          -- OTHER
        , hasSelection
        )

{-| AnnotationSet aims at managing a set of annotations.
Basically, it is an array of Annotation.
So anything that can be done on arrays can be done on an AnnotationSet.

# Model
@docs AnnotationSet

# Update
@docs remove

# View
@docs viewAllSelections, selectTag

# Output
@docs object, pathsObject

# Other
@docs hasSelection
-}

import Html as H exposing (Html)
import Svg exposing (Svg)
import Json.Encode as JE
import Array exposing (Array)
import Annotation as Ann exposing (Annotation)
import Helpers.Views as HPV
import Helpers.Array as HPA


-- MODEL #############################################################


{-| An annotation set is basically an array of annotations.
-}
type alias AnnotationSet =
    Array Annotation



-- UPDATE ############################################################


{-| Remove the annotation at the given id from the set.
-}
remove : Int -> AnnotationSet -> AnnotationSet
remove id set =
    HPA.removeAt id set



-- VIEW ##############################################################


{-| View of the SVG representation of all the annotations in the set.
-}
viewAllSelections : AnnotationSet -> List (Svg msg)
viewAllSelections set =
    Array.map Ann.selectionView set
        |> Array.toList


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


{-| JavaScript object representing an annotation set.
-}
object : AnnotationSet -> JE.Value
object set =
    JE.array <| Array.map Ann.object set


{-| Simplified JavaScript object representing the selection paths of the annotation set.
-}
pathsObject : AnnotationSet -> JE.Value
pathsObject set =
    JE.array <| Array.map Ann.pathObject set



-- OTHER #############################################################


{-| Indicates if the annotation set has at least one selection.
-}
hasSelection : AnnotationSet -> Bool
hasSelection set =
    List.any Ann.hasSelection <| Array.toList set
