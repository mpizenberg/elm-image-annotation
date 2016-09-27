-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/

module AnnotationSet exposing (..)


{-| AnnotationSet aims at managing a set of annotations.
-}


import Dict
import Svg
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import String
import Json.Decode as Json


import Annotation as Ann
import Utils.Helpers as HP




-- MODEL #############################################################




type alias Model_ =
    { annotations : Dict.Dict Int Ann.Model
    , selected : Maybe Int
    , uid : Int
    }


type Model = AnnSet Model_


init : (Model, Cmd msg)
init = (AnnSet <| Model_ Dict.empty Nothing 0, Cmd.none)




-- UPDATE ############################################################




type Msg
    = CreateAnnotation
    | Delete
    | Select (Maybe Int)
    | Annotation Int Ann.Msg
    | Annotate Ann.Msg


update : Msg -> Model -> (Model, Cmd Msg)
update msg (AnnSet model) =
    case msg of
        CreateAnnotation ->
            let
                (annotation, _) =
                    Ann.init Nothing <| Just <| toString model.uid
            in
                ( AnnSet { model
                    | annotations = Dict.insert
                        model.uid
                        annotation
                        model.annotations
                    , selected = Just model.uid
                    , uid = model.uid + 1
                    }
                , Cmd.none
                )
        Delete ->
            let
                annotations = case model.selected of
                    Nothing ->
                        model.annotations
                    Just id ->
                        Dict.remove id model.annotations
            in
                ( AnnSet {model | annotations = annotations }
                , Cmd.none
                )
        Select id ->
            ( AnnSet { model | selected = id }
            , Cmd.none
            )
        Annotation id annMsg ->
            let
                maybeAnn = Dict.get id model.annotations
            in
                case maybeAnn of
                    Nothing ->
                        (AnnSet model, Cmd.none)
                    Just ann ->
                        let
                            (ann', cmdMsg) = Ann.update annMsg ann
                            annotations =
                                Dict.insert id ann' model.annotations
                        in
                            ( AnnSet {model | annotations = annotations}
                            , Cmd.map (Annotation id) cmdMsg
                            )
        Annotate annMsg ->
            case model.selected of
                Nothing ->
                    (AnnSet model, Cmd.none)
                Just id ->
                    update (Annotation id annMsg) (AnnSet model)




-- VIEW ##############################################################




selectionsView : Model -> List (Svg.Svg msg)
selectionsView (AnnSet model) =
    List.map Ann.selectionView <| Dict.values model.annotations


selectHtml : Model -> H.Html Msg
selectHtml (AnnSet model) =
    H.select
        [HP.onChange <| Select << Result.toMaybe << String.toInt]
        ((H.option
            [HA.value "none", HA.selected (model.selected == Nothing)]
            [H.text "None"])
        ::
        (List.map
            (Ann.optionTag model.selected) <|
            Dict.toList model.annotations
        ))
