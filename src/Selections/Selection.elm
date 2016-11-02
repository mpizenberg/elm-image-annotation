-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/

module Selections.Selection exposing (..)


{-| The Selection module aims at regrouping functions useful to all kind of selections
-}


import Svg
import Svg.Attributes as SvgA
import Json.Encode as JE
import Time exposing (Time)




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


type alias Timings = ( Maybe Time, Maybe Time )


type alias Selection =
    { style : Style
    , timings : Timings
    , pointerEvents : Bool
    }


defaultSelection : Selection
defaultSelection = Selection defaultStyle (Nothing, Nothing) False




-- UPDATE ############################################################




changeStyle : Maybe String -> Maybe Int -> Maybe Bool -> Style -> Style
changeStyle color strokeWidth highlighted oldStyle =
    let
        color' = Maybe.withDefault oldStyle.color color
        strokeWidth' = Maybe.withDefault oldStyle.strokeWidth strokeWidth
        highlighted' = Maybe.withDefault oldStyle.highlighted highlighted
    in
        Style color' strokeWidth' highlighted'


changeSelectionStyle : Maybe String -> Maybe Int -> Maybe Bool -> Selection -> Selection
changeSelectionStyle color strokeWidth highlighted selection =
    { selection | style = changeStyle color strokeWidth highlighted selection.style }


startTime : Maybe Time -> Timings -> Timings
startTime t timings = ( t, snd timings )


stopTime : Maybe Time -> Timings -> Timings
stopTime t timings = ( fst timings, t )


duration : Timings -> Maybe Time
duration timings =
    case timings of
        (Nothing, _) -> Nothing
        (_, Nothing) -> Nothing
        (Just start, Just stop) -> Just (stop - start)




-- VIEW ##############################################################




selectionAttributes : Selection -> List (Svg.Attribute msg)
selectionAttributes selection =
    ( SvgA.pointerEvents <| if selection.pointerEvents then "auto" else "none" )
    :: ( styleAttributes selection.style )


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


selectionObject : Selection -> JE.Value
selectionObject selection =
    JE.object
        [ ("style", styleObject selection.style)
        , ("timings", timingsObject selection.timings)
        , ("pointerEvents", JE.bool selection.pointerEvents)
        ]


timingsObject : Timings -> JE.Value
timingsObject (start, stop) =
    let
        maybeTimeObject : Maybe Float -> JE.Value
        maybeTimeObject maybeTime =
            case maybeTime of
                Nothing -> JE.null
                Just time -> JE.float time
    in
        JE.list <| List.map maybeTimeObject [start, stop]


styleObject : Style -> JE.Value
styleObject style =
    JE.object
        [ ("color", JE.string style.color)
        , ("strokeWidth", JE.int style.strokeWidth)
        , ("highlighted", JE.bool style.highlighted)
        ]
