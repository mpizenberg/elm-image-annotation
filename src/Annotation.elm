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
import Tools exposing (Tool)


-- MODEL #############################################################


type Selection
    = NoSelection
    | RSel Rectangle
    | OSel Outline


type alias Annotation =
    { selection : Selection
    , label : String
    }


default : Annotation
default =
    { selection = NoSelection
    , label = "No label"
    }



-- UPDATE ############################################################


update : ( Int, Int ) -> ( Int, Int ) -> Tool -> Annotation -> Annotation
update origin newPos tool annotation =
    case tool of
        Tools.None ->
            annotation

        Tools.Rectangle ->
            let
                rectangle =
                    case annotation.selection of
                        NoSelection ->
                            SR.defaultRectangle

                        RSel rect ->
                            rect

                        OSel outline ->
                            SR.defaultRectangle |> SR.changeSel outline.selection
            in
                { annotation | selection = RSel <| SR.update origin newPos rectangle }

        Tools.Outline ->
            -- TODO
            annotation



-- VIEW ##############################################################


selectionView : Annotation -> Svg msg
selectionView { selection } =
    case selection of
        NoSelection ->
            Svg.text "No Selection"

        RSel rect ->
            SR.view rect

        OSel outline ->
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
                    NoSelection ->
                        ": No Selection"

                    RSel _ ->
                        ": Rectangle"

                    OSel _ ->
                        ": Outline"
        ]



-- OUTPUTS ##############################################################


object : Annotation -> JE.Value
object annotation =
    JE.object
        [ ( "selection"
          , case annotation.selection of
                NoSelection ->
                    JE.null

                RSel rect ->
                    JE.object [ ( "Rectangle", SR.object rect ) ]

                OSel outline ->
                    JE.object [ ( "Outline", SO.object outline ) ]
          )
        , ( "label", JE.string annotation.label )
        ]


pathObject : Annotation -> JE.Value
pathObject annotation =
    case annotation.selection of
        NoSelection ->
            JE.null

        RSel rect ->
            SR.pathObject rect

        OSel outline ->
            SO.pathObject outline



-- OTHER #############################################################


{-| Indicates if the annotation has a selection
-}
hasSelection : Annotation -> Bool
hasSelection annotation =
    annotation.selection /= NoSelection
