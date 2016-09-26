-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/

module Annotation exposing (..)


{-| An annotation is the combination of a selection and a label.
-}


import Svg


import Selections.RectangleSelection as RS
import Selections.OutlineSelection as OS




-- MODEL #############################################################




type SelectionModel
    = RSModel RS.Model
    | OSModel OS.Model


type SelectionMsg
    = RSMsg RS.Msg
    | OSMsg OS.Msg


type alias Model_ =
    { selection : Maybe SelectionModel
    , label : Maybe String
    }


type Model = Annotation Model_


init : Maybe SelectionModel -> Maybe String -> (Model, Cmd msg)
init selModel label =
    ( Annotation <| Model_ selModel label
    , Cmd.none
    )




-- UPDATE ############################################################




type Msg
    = Selection (Maybe SelectionMsg)
    | Label (Maybe String)


update : Msg -> Model -> (Model, Cmd Msg)
update msg (Annotation model) =
    case msg of
        Label label ->
            (Annotation {model | label = label}, Cmd.none)
        Selection selMsg ->
            case selMsg of
                Nothing ->
                    ( Annotation <| Model_ Nothing model.label
                    , Cmd.none
                    )
                Just (RSMsg rsMsg) ->
                    (Annotation model, Cmd.none)
                Just (OSMsg osMsg) ->
                    (Annotation model, Cmd.none)




-- VIEW ##############################################################




selectionView : Model -> Svg.Svg msg
selectionView (Annotation model) =
    Svg.text "Hello World!"
