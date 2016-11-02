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


import Selections.Selection as Sel




-- MODEL #############################################################




type alias Model_ =
    { path : List Sel.Pos
    , style : Sel.Style
    , pointerEvents : Bool
    }


type Model = Model Model_


init : (Model, Cmd Msg)
init =
    ( Model <| Model_ [] Sel.defaultStyle False
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
    | TriggerPointerEvents Bool


update : Msg -> Model -> (Model, Cmd Msg)
update msg (Model model) =
    case msg of
        Style color strokeWidth highlighted ->
            ( Model { model
                | style = Sel.changeStyle color strokeWidth highlighted model.style
                }
            , Cmd.none
            )
        AddPoint (x, y) ->
            ( Model { model | path = (Sel.Pos x y) :: model.path }, Cmd.none )
        Reset ->
            ( Model { model | path = [] }, Cmd.none )
        ResetWithPoint (x, y) ->
            ( Model { model | path = [Sel.Pos x y] }, Cmd.none )
        TriggerPointerEvents bool ->
            ( Model { model | pointerEvents = bool }, Cmd.none )




-- VIEW ##############################################################




view : Model -> Svg.Svg msg
view (Model model) =
    Svg.polygon
        ( Sel.styleAttributes model.style
        ++
        [ SvgA.pointerEvents <| if model.pointerEvents then "auto" else "none"
        , SvgA.points <| pathToString model.path
        ]) []


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
        , ("style", Sel.styleObject model.style)
        , ("pointerEvents", JE.bool model.pointerEvents)
        ]


pathObject : Model -> JE.Value
pathObject (Model model) =
    JE.list <| List.map Sel.posPathObject model.path
