-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module Selections.Selection exposing (..)

{-| The Selection module aims at regrouping functions useful to all kind of selections
-}

import Svg exposing (Svg)
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
    , strokeWidth : Float
    , highlighted : Bool
    }


defaultStyle : Style
defaultStyle =
    { color = "red"
    , strokeWidth = 3
    , highlighted = False
    }


type alias Timings =
    ( Maybe Time, Maybe Time )


type alias Selection =
    { style : Style
    , timings : Timings
    , pointerEvents : Bool
    }


defaultSelection : Selection
defaultSelection =
    { style = defaultStyle
    , timings = ( Nothing, Nothing )
    , pointerEvents = False
    }



-- UPDATE ############################################################


changeSel : Selection -> { a | selection : Selection } -> { a | selection : Selection }
changeSel selCommon selection =
    { selection | selection = selCommon }


changeStrokeWidth : Float -> Selection -> Selection
changeStrokeWidth width selection =
    let
        oldStyle =
            selection.style
    in
        { selection | style = { oldStyle | strokeWidth = width } }


setStartTime : Maybe Time -> Selection -> Selection
setStartTime t selection =
    { selection | timings = ( t, Tuple.second selection.timings ) }


setStopTime : Maybe Time -> Selection -> Selection
setStopTime t selection =
    { selection | timings = ( Tuple.first selection.timings, t ) }


resetTimings : Selection -> Selection
resetTimings selection =
    { selection | timings = ( Nothing, Nothing ) }



-- VIEW ##############################################################


selectionAttributes : Selection -> List (Svg.Attribute msg)
selectionAttributes selection =
    pointerEventsAttribute selection.pointerEvents
        :: (styleAttributes selection.style)


pointerEventsAttribute : Bool -> Svg.Attribute msg
pointerEventsAttribute pointerEvents =
    SvgA.pointerEvents <|
        if pointerEvents then
            "auto"
        else
            "none"


styleAttributes : Style -> List (Svg.Attribute msg)
styleAttributes style =
    [ SvgA.stroke style.color
    , SvgA.fill style.color
    , SvgA.strokeWidth (toString style.strokeWidth)
    , SvgA.fillOpacity <| opacityAttr style.highlighted
    ]


opacityAttr : Bool -> String
opacityAttr highlighted =
    if highlighted then
        "0.5"
    else
        "0"



-- OUTPUTS ##############################################################


maybeTimeObject : Maybe Time -> JE.Value
maybeTimeObject maybeTime =
    case maybeTime of
        Nothing ->
            JE.null

        Just time ->
            JE.float time


duration : Timings -> Maybe Time
duration timings =
    case timings of
        ( Nothing, _ ) ->
            Nothing

        ( _, Nothing ) ->
            Nothing

        ( Just start, Just stop ) ->
            Just (stop - start)


posObject : Pos -> JE.Value
posObject pos =
    JE.object
        [ ( "x", JE.int pos.x )
        , ( "y", JE.int pos.y )
        ]


posPathObject : Pos -> JE.Value
posPathObject pos =
    JE.list [ JE.int pos.x, JE.int pos.y ]


sizeObject : Size -> JE.Value
sizeObject size =
    JE.object
        [ ( "width", JE.int size.width )
        , ( "height", JE.int size.height )
        ]


selectionObject : Selection -> JE.Value
selectionObject selection =
    JE.object
        [ ( "style", styleObject selection.style )
        , ( "timings", timingsObject selection.timings )
        , ( "pointerEvents", JE.bool selection.pointerEvents )
        ]


timingsObject : Timings -> JE.Value
timingsObject ( start, stop ) =
    JE.list <| List.map maybeTimeObject [ start, stop ]


styleObject : Style -> JE.Value
styleObject style =
    JE.object
        [ ( "color", JE.string style.color )
        , ( "strokeWidth", JE.float style.strokeWidth )
        , ( "highlighted", JE.bool style.highlighted )
        ]
