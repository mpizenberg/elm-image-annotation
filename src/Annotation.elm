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
    { selection : SelectionModel
    , label : String
    }


type Model = Annotation Model_




-- UPDATE ############################################################




type Msg
    = Selection SelectionMsg
    | Label String




-- VIEW ##############################################################




selectionView : Model -> Svg.Svg msg
selectionView (Annotation model) =
    Svg.text "Hello World!"
