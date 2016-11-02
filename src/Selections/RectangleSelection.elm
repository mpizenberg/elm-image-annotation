-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/

module Selections.RectangleSelection exposing
    (..)


{-| RectangleSelection contains the tools to manipule rectangle selections.
-}


import Svg
import Svg.Attributes as SvgA
import Json.Encode as JE


import Selections.Selection as Sel




-- MODEL #############################################################




type alias Geometry =
    { pos : Sel.Pos
    , size : Sel.Size
    }


type alias Model_ =
    { selection : Sel.Selection
    , geometry : Geometry
    }


type Model = Model Model_


init : (Int, Int) -> (Int, Int) -> (Model, Cmd Msg)
init (left, top) (width, height) =
    ( Model <| Model_
        Sel.defaultSelection
        (Geometry (Sel.Pos left top) (Sel.Size width height))
    , Cmd.none
    )


defaultModel : Model
defaultModel =
    fst <| init (0, 0) (0, 0)




-- UPDATE ############################################################




type Msg
    = Style (Maybe String) (Maybe Int) (Maybe Bool)
    | Geom (Maybe (Int, Int)) (Maybe (Int, Int))


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
        Geom pos size ->
            ( Model {model | geometry = changeGeometry pos size model.geometry}
            , Cmd.none
            )


changeGeometry : Maybe (Int, Int) -> Maybe (Int, Int) -> Geometry -> Geometry
changeGeometry pos size geom =
    let
        (x, y) = Maybe.withDefault (geom.pos.x, geom.pos.y) pos
        (width, height) = Maybe.withDefault (geom.size.width, geom.size.height) size
    in
        Geometry (Sel.Pos x y) (Sel.Size width height)




-- VIEW ##############################################################




view : Model -> Svg.Svg msg
view (Model model) =
    Svg.rect
        ( Sel.selectionAttributes model.selection
        ++
        [ SvgA.x (toString model.geometry.pos.x)
        , SvgA.y (toString model.geometry.pos.y)
        , SvgA.width (toString model.geometry.size.width)
        , SvgA.height (toString model.geometry.size.height)
        ]) []




-- OUTPUTS ##############################################################




object : Model -> JE.Value
object (Model model) =
    JE.object
        [ ("geometry", geomObject model.geometry)
        , ("selection", Sel.selectionObject model.selection)
        ]


geomObject : Geometry -> JE.Value
geomObject geom =
    JE.object
        [ ("pos", Sel.posObject geom.pos)
        , ("size", Sel.sizeObject geom.size)
        ]


pathObject : Model -> JE.Value
pathObject (Model model) =
    let
        -- sides
        left = model.geometry.pos.x
        top = model.geometry.pos.y
        right = left + model.geometry.size.width
        bottom = top + model.geometry.size.height
        -- corners
        top_left = Sel.posPathObject <| Sel.Pos left top
        bottom_left = JE.list [JE.int left, JE.int bottom]
        bottom_right = JE.list [JE.int right, JE.int bottom]
        top_right = JE.list [JE.int right, JE.int top]
    in
        JE.list
            [ top_left, bottom_left, bottom_right, top_right ]
