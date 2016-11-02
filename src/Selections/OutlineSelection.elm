-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/

module Selections.OutlineSelection exposing
    (..)


{-| OutlineSelection contains the tools to manipule outline selections.
-}


import Svg
import Svg.Attributes as SvgA
import Html.App as App
import String
import Json.Encode as JE
import Time exposing (Time)
import Task


import Selections.Selection as Sel




-- MODEL #############################################################




type alias Model_ =
    { selection : Sel.Selection
    , path : List Sel.Pos
    }


type Model = Model Model_


init : (Model, Cmd Msg)
init =
    ( Model <| Model_ Sel.defaultSelection []
    , Cmd.none
    )


defaultModel : Model
defaultModel =
    fst init




-- UPDATE ############################################################




type Msg
    = Style (Maybe String) (Maybe Int) (Maybe Bool)
    | AddPoint (Int, Int)
    | Reset
    | ResetWithPoint (Int, Int)
    | StartTime (Maybe Time)
    | StopTime (Maybe Time)


update : Msg -> Model -> (Model, Cmd Msg)
update msg (Model model) =
    case msg of
        Style color strokeWidth highlighted ->
            ( Model { model
                | selection = Sel.changeSelectionStyle
                    color
                    strokeWidth
                    highlighted
                    model.selection
                }
            , Cmd.none
            )
        AddPoint (x, y) ->
            ( Model { model | path = (Sel.Pos x y) :: model.path }
            -- Very bad performances but will do for now
            , Task.perform (identity) (StopTime << Just) Time.now
            )
        Reset ->
            ( Model { model | path = [], selection = Sel.resetTimings model.selection }
            , Cmd.none
            )
        ResetWithPoint (x, y) ->
            ( Model { model
                | path = [Sel.Pos x y]
                , selection = Sel.resetTimings model.selection
                }
            , Task.perform (identity) (StartTime << Just) Time.now
            )
        StartTime time ->
            ( Model { model | selection = Sel.startSelectionTime time model.selection }
            , Cmd.none
            )
        StopTime time ->
            ( Model { model | selection = Sel.stopSelectionTime time model.selection }
            , Cmd.none
            )




-- VIEW ##############################################################




view : Model -> Svg.Svg msg
view (Model model) =
    Svg.polygon
        ( (SvgA.points <| pathToString model.path)
        :: Sel.selectionAttributes model.selection
        ) []


pathToString : List Sel.Pos -> String
pathToString positions =
    String.join " " <| List.map posToString positions


posToString : Sel.Pos -> String
posToString {x, y} = toString x ++ "," ++ toString y




-- OUTPUTS ##############################################################




object : Model -> JE.Value
object (Model model) =
    JE.object
        [ ("path", JE.list <| List.map Sel.posObject model.path)
        , ("selection", Sel.selectionObject model.selection)
        ]


pathObject : Model -> JE.Value
pathObject (Model model) =
    JE.object
        [ ("duration", Sel.maybeTimeObject <| Sel.duration model.selection.timings)
        , ("path", JE.list <| List.map Sel.posPathObject model.path)
        ]
