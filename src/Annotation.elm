-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module Annotation exposing (..)

{-| An annotation is the combination of a selection and a label.
-}

import Html as H exposing (Html)
import Html.Attributes as HA
import Svg exposing (Svg)
import Json.Encode as JE
import Selections.Rectangle as SR exposing (Rectangle)
import Selections.Outline as SO exposing (Outline)


-- MODEL #############################################################


type Selection
    = RSel Rectangle
    | OSel Outline


type alias Annotation =
    { selection : Maybe Selection
    , label : Maybe String
    }


emptyAnnotation : Annotation
emptyAnnotation =
    { selection = Nothing
    , label = Nothing
    }



-- VIEW ##############################################################


selectionView : Annotation -> Svg msg
selectionView { selection } =
    case selection of
        Nothing ->
            Svg.text "No Selection"

        Just (RSel rect) ->
            SR.view rect

        Just (OSel outline) ->
            SO.view outline


{-| An <option> tag to be put in a <select> tag.
   currentId is the id of the currently selected option.
-}
optionTag : Maybe Int -> ( Int, Annotation ) -> Html msg
optionTag currentId ( id, { selection } ) =
    H.option
        [ HA.value (toString id), HA.selected (currentId == Just id) ]
        [ H.text <|
            toString id
                ++ case selection of
                    Nothing ->
                        ": No Selection"

                    Just (RSel _) ->
                        ": Rectangle"

                    Just (OSel _) ->
                        ": Outline"
        ]



-- OUTPUTS ##############################################################


object : Annotation -> JE.Value
object annotation =
    JE.object
        [ ( "selection"
          , case annotation.selection of
                Nothing ->
                    JE.null

                Just (RSel rect) ->
                    JE.object [ ( "Rectangle", SR.object rect ) ]

                Just (OSel outline) ->
                    JE.object [ ( "Outline", SO.object outline ) ]
          )
        , ( "label"
          , case annotation.label of
                Nothing ->
                    JE.null

                Just label ->
                    JE.string label
          )
        ]


pathObject : Annotation -> JE.Value
pathObject annotation =
    case annotation.selection of
        Nothing ->
            JE.null

        Just (RSel rect) ->
            SR.pathObject rect

        Just (OSel outline) ->
            SO.pathObject outline



-- OTHER #############################################################


{-| Indicates if the annotation has a selection
-}
hasSelection : Annotation -> Bool
hasSelection annotation =
    annotation.selection /= Nothing
