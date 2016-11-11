module Pointer exposing (..)

import Html as H exposing (Html)
import Html.Events as HE
import Helpers.Events as HPE
import Tools exposing (Tool)


type Event
    = Down
    | Move
    | Up
    | Cancel


type alias Pointer =
    { event : Event
    , offsetX : Int
    , offsetY : Int
    , movementX : Int
    , movementY : Int
    }


attributes : (Pointer -> msg) -> Tool -> Maybe Pointer -> List (H.Attribute msg)
attributes msgMaker currentTool previousPointer =
    case currentTool of
        Tools.None ->
            []

        _ ->
            autoAttributes HPE.offsetOn msgMaker currentTool previousPointer


autoAttributes :
    (String -> (( Int, Int ) -> msg) -> H.Attribute msg)
    -> (Pointer -> msg)
    -> Tool
    -> Maybe Pointer
    -> List (H.Attribute msg)
autoAttributes eventDetector msgMaker currentTool previousPointer =
    [ eventDetector "mousedown" <| msgMaker << (pointerOffset Down)
    , eventDetector "mouseup" <| msgMaker << (pointerOffset Up)
    ]
        ++ if previousPointer == Nothing then
            []
           else
            [ eventDetector "mousemove" <| msgMaker << (pointerOffset Move) ]


pointerOffset : Event -> ( Int, Int ) -> Pointer
pointerOffset event ( offsetX, offsetY ) =
    { event = event
    , offsetX = offsetX
    , offsetY = offsetY
    , movementX = 0
    , movementY = 0
    }


pointerMovement : Event -> ( Int, Int ) -> Pointer
pointerMovement event ( movementX, movementY ) =
    { event = event
    , offsetX = 0
    , offsetY = 0
    , movementX = movementX
    , movementY = movementY
    }
