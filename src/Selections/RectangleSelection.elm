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
    { geometry : Geometry
    , style : Sel.Style
    , pointerEvents : Bool
    }


type Model = RectSel Model_


init : (Int, Int) -> (Int, Int) -> (Model, Cmd Msg)
init (left, top) (width, height) =
    ( RectSel <| Model_
        (Geometry (Sel.Pos left top) (Sel.Size width height))
        Sel.defaultStyle
        False
    , Cmd.none
    )


defaultModel : Model
defaultModel =
    fst <| init (0, 0) (0, 0)




-- UPDATE ############################################################




type Msg
    = Style (Maybe String) (Maybe Int) (Maybe Bool)
    | Geom (Maybe (Int, Int)) (Maybe (Int, Int))
    | TriggerPointerEvents Bool


update : Msg -> Model -> (Model, Cmd Msg)
update msg (RectSel model) =
    case msg of
        Style color strokeWidth highlighted ->
            ( RectSel { model
                | style = Sel.changeStyle color strokeWidth highlighted model.style
                }
            , Cmd.none
            )
        Geom pos size ->
            ( RectSel {model | geometry = changeGeometry pos size model.geometry}
            , Cmd.none
            )
        TriggerPointerEvents bool ->
            ( RectSel { model | pointerEvents = bool }, Cmd.none )


changeGeometry : Maybe (Int, Int) -> Maybe (Int, Int) -> Geometry -> Geometry
changeGeometry pos size geom =
    let
        (x, y) = Maybe.withDefault (geom.pos.x, geom.pos.y) pos
        (width, height) = Maybe.withDefault (geom.size.width, geom.size.height) size
    in
        Geometry (Sel.Pos x y) (Sel.Size width height)




-- VIEW ##############################################################




view : Model -> Svg.Svg msg
view (RectSel model) =
    Svg.rect
        ( Sel.styleAttributes model.style
        ++
        [ SvgA.x (toString model.geometry.pos.x)
        , SvgA.y (toString model.geometry.pos.y)
        , SvgA.width (toString model.geometry.size.width)
        , SvgA.height (toString model.geometry.size.height)
        , SvgA.pointerEvents (if model.pointerEvents then "auto" else "none")
        ]) []




-- OUTPUTS ##############################################################




object : Model -> JE.Value
object (RectSel model) =
    JE.object
        [ ("geometry", geomObject model.geometry)
        , ("style", Sel.styleObject model.style)
        , ("pointerEvents", JE.bool model.pointerEvents)
        ]


geomObject : Geometry -> JE.Value
geomObject geom =
    JE.object
        [ ("pos", Sel.posObject geom.pos)
        , ("size", Sel.sizeObject geom.size)
        ]
