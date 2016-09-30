-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/

module Selections.Selection exposing (..)


{-| The Selection module aims at regrouping functions useful to all kind of selections
-}


import Svg
import Svg.Attributes as SvgA
import Json.Encode as JE




-- MODEL #############################################################




type alias Pos =
    { x : Int
    , y : Int
    }


type alias Size =
    { width : Int
    , height : Int
    }


type alias Style =
    { color : String
    , strokeWidth : Int
    , highlighted : Bool
    }


defaultStyle : Style
defaultStyle =
    Style "red" 3 False




-- UPDATE ############################################################




changeStyle : Maybe String -> Maybe Int -> Maybe Bool -> Style -> Style
changeStyle color strokeWidth highlighted oldStyle =
    let
        color' = Maybe.withDefault oldStyle.color color
        strokeWidth' = Maybe.withDefault oldStyle.strokeWidth strokeWidth
        highlighted' = Maybe.withDefault oldStyle.highlighted highlighted
    in
        Style color' strokeWidth' highlighted'




-- VIEW ##############################################################




styleAttributes : Style -> List (Svg.Attribute msg)
styleAttributes style =
    let
        opacityAttr = if style.highlighted then "0.5" else "0"
    in
        [ SvgA.stroke style.color
        , SvgA.fill style.color
        , SvgA.strokeWidth (toString style.strokeWidth)
        , SvgA.fillOpacity opacityAttr
        ]




-- OUTPUTS ##############################################################




posObject : Pos -> JE.Value
posObject pos =
    JE.object
        [ ("x", JE.int pos.x)
        , ("y", JE.int pos.y)
        ]


posPathObject : Pos -> JE.Value
posPathObject pos =
    JE.list [JE.int pos.x, JE.int pos.y]


sizeObject : Size -> JE.Value
sizeObject size =
    JE.object
        [ ("width", JE.int size.width)
        , ("height", JE.int size.height)
        ]


styleObject : Style -> JE.Value
styleObject style =
    JE.object
        [ ("color", JE.string style.color)
        , ("strokeWidth", JE.int style.strokeWidth)
        , ("highlighted", JE.bool style.highlighted)
        ]
