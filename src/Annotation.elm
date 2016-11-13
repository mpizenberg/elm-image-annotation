-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module Annotation
    exposing
        ( Selection(..)
        , Event(..)
        , Annotation
        , default
          -- UPDATE
        , setStartTime
        , setStopTime
        , setLabel
        , updateSelection
          -- VIEW
        , selectionView
          -- OUTPUTS
        , object
        , pathObject
          -- OTHERS
        , hasSelection
        )

{-| An annotation is the combination of a selection and a label.

# Model
@docs Selection, Event, Annotation, default

# Update
@docs setStartTime, setStopTime, setLabel, updateSelection

# View
@docs selectionView

# Outputs
@docs object, pathObject

# Others
@docs hasSelection
-}

import Html as H exposing (Html)
import Html.Attributes as HA
import Svg exposing (Svg)
import Json.Encode as JE
import Selections.Selection as Sel
import Selections.Rectangle as SR exposing (Rectangle)
import Selections.Outline as SO exposing (Outline)
import Tools exposing (Tool)
import Time exposing (Time)


-- MODEL #############################################################


{-| What can be the selection of an annotation.
-}
type Selection
    = NoSelection
    | RSel Rectangle
    | OSel Outline


{-| Event marking the starting of a new selection or just the continuation of the current one.
-}
type Event
    = Start
    | Continue


{-| An annotation is composed of a selection and has a label.
-}
type alias Annotation =
    { selection : Selection
    , label : String
    }


{-| Default empty annotation.
-}
default : Annotation
default =
    { selection = NoSelection
    , label = "No label"
    }



-- UPDATE ############################################################


{-| Set the start time of the annotation.
-}
setStartTime : Maybe Time -> Annotation -> Annotation
setStartTime maybeTime annotation =
    case annotation.selection of
        NoSelection ->
            annotation

        RSel rect ->
            { annotation | selection = RSel <| SR.setStartTime maybeTime rect }

        OSel outline ->
            { annotation | selection = OSel <| SO.setStartTime maybeTime outline }


{-| Set the stop time of the annotation.
-}
setStopTime : Maybe Time -> Annotation -> Annotation
setStopTime maybeTime annotation =
    case annotation.selection of
        NoSelection ->
            annotation

        RSel rect ->
            { annotation | selection = RSel <| SR.setStopTime maybeTime rect }

        OSel outline ->
            { annotation | selection = OSel <| SO.setStopTime maybeTime outline }


{-| Set the label of the annotation.
-}
setLabel : String -> Annotation -> Annotation
setLabel label annotation =
    { annotation | label = label }


{-| Update the selection of the annotation depending on the type of event,
the corner positions and the current tool.
-}
updateSelection : Event -> ( Int, Int ) -> ( Int, Int ) -> Tool -> Annotation -> Annotation
updateSelection event origin newPos tool annotation =
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
                            SR.defaultRectangle |> Sel.changeSel outline.selection
            in
                { annotation | selection = RSel <| SR.update origin newPos rectangle }

        Tools.Outline ->
            let
                outline =
                    case annotation.selection of
                        NoSelection ->
                            SO.defaultOutline

                        RSel rect ->
                            SO.defaultOutline |> Sel.changeSel rect.selection

                        OSel oldOutline ->
                            case event of
                                Start ->
                                    SO.resetPath oldOutline

                                _ ->
                                    oldOutline
            in
                { annotation | selection = OSel <| SO.addPoint newPos outline }



-- VIEW ##############################################################


{-| Svg view representing the annotation.
-}
selectionView : Annotation -> Svg msg
selectionView { selection } =
    case selection of
        NoSelection ->
            Svg.text "No Selection"

        RSel rect ->
            SR.view rect

        OSel outline ->
            SO.view outline



-- OUTPUTS ##############################################################


{-| Return JS object representing the annotation.
-}
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


{-| Return JS object simplified version of the path only.
-}
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


{-| Indicates if the annotation has a selection.
-}
hasSelection : Annotation -> Bool
hasSelection annotation =
    annotation.selection /= NoSelection
