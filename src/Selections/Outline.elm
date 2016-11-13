-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module Selections.Outline exposing (..)

{-| Outline contains the tools to manipule outline selections.
-}

import Svg exposing (Svg)
import Svg.Attributes as SvgA
import String
import Json.Encode as JE
import Time exposing (Time)
import Selections.Selection as Sel exposing (Selection)


-- MODEL #############################################################


type alias Outline =
    { selection : Selection
    , path : List Sel.Pos
    }


defaultOutline : Outline
defaultOutline =
    { selection = Sel.defaultSelection
    , path = []
    }



-- UPDATE ############################################################


changeStrokeWidth : Float -> Outline -> Outline
changeStrokeWidth width outline =
    { outline | selection = Sel.changeStrokeWidth width outline.selection }


addPoint : ( Int, Int ) -> Outline -> Outline
addPoint ( x, y ) outline =
    { outline | path = (Sel.Pos x y) :: outline.path }


resetPath : Outline -> Outline
resetPath outline =
    { outline | path = [] }


resetTimings : Outline -> Outline
resetTimings outline =
    { outline | selection = Sel.resetTimings outline.selection }


setStartTime : Maybe Time -> Outline -> Outline
setStartTime t outline =
    { outline | selection = Sel.setStartTime t outline.selection }


setStopTime : Maybe Time -> Outline -> Outline
setStopTime t outline =
    { outline | selection = Sel.setStopTime t outline.selection }



-- VIEW ##############################################################


view : Outline -> Svg msg
view outline =
    Svg.polygon
        ((SvgA.points <| pathToString outline.path)
            :: Sel.selectionAttributes outline.selection
        )
        []


pathToString : List Sel.Pos -> String
pathToString positions =
    String.join " " <| List.map posToString positions


posToString : Sel.Pos -> String
posToString { x, y } =
    toString x ++ "," ++ toString y



-- OUTPUTS ##############################################################


object : Outline -> JE.Value
object outline =
    JE.object
        [ ( "path", JE.list <| List.map Sel.posObject outline.path )
        , ( "selection", Sel.selectionObject outline.selection )
        ]


pathObject : Outline -> JE.Value
pathObject outline =
    JE.object
        [ ( "duration", Sel.maybeTimeObject <| Sel.duration outline.selection.timings )
        , ( "path", JE.list <| List.map Sel.posPathObject outline.path )
        ]
