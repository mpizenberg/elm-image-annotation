-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/

module Annotation exposing (..)


{-| An annotation is the combination of a selection and a label.
-}


import Html as H
import Html.Attributes as HA
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
                    updateRS rsMsg model
                Just (OSMsg osMsg) ->
                    updateOS osMsg model


updateRS : RS.Msg -> Model_ -> (Model, Cmd Msg)
updateRS rsMsg model =
    case model.selection of
        Nothing ->
            updateRS
                rsMsg
                {model | selection = Just (RSModel RS.defaultModel)}
        Just (OSModel _) ->
            updateRS rsMsg {model | selection = Nothing}
        Just (RSModel rsModel) ->
            let
                (rsModel', cmdRsMsg) = RS.update rsMsg rsModel
            in
                ( Annotation
                    {model | selection = Just (RSModel rsModel')}
                , Cmd.map (Selection << Just << RSMsg) cmdRsMsg
                )


updateOS : OS.Msg -> Model_ -> (Model, Cmd Msg)
updateOS osMsg model =
    case model.selection of
        Nothing ->
            updateOS
                osMsg
                {model | selection = Just (OSModel OS.defaultModel)}
        Just (RSModel _) ->
            updateOS osMsg {model | selection = Nothing}
        Just (OSModel osModel) ->
            let
                (osModel', cmdOsMsg) = OS.update osMsg osModel
            in
                ( Annotation
                    {model | selection = Just (OSModel osModel')}
                , Cmd.map (Selection << Just << OSMsg) cmdOsMsg
                )




-- VIEW ##############################################################




selectionView : Model -> Svg.Svg msg
selectionView (Annotation model) =
    case model.selection of
        Nothing ->
            Svg.text "No Selection"
        Just (RSModel rsModel) ->
            RS.view rsModel
        Just (OSModel osModel) ->
            OS.view osModel


optionTag : Maybe Int -> (Int, Model) -> H.Html msg
optionTag currentId (id, (Annotation model)) =
    H.option
        [ HA.value (toString id), HA.selected (currentId == Just id)]
        [ H.text <| toString id ++
            case model.selection of
                Nothing -> ": No Selection"
                Just (RSModel _) -> ": Rectangle"
                Just (OSModel _) -> ": Outline"
        ]
