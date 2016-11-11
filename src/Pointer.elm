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
            mouseAttributes HPE.movementOn pointerMovement msgMaker currentTool previousPointer

        _ ->
            mouseAttributes HPE.offsetOn pointerOffset msgMaker currentTool previousPointer


mouseAttributes :
    (String -> (( Int, Int ) -> msg) -> H.Attribute msg)
    -> (Event -> ( Int, Int ) -> Pointer)
    -> (Pointer -> msg)
    -> Tool
    -> Maybe Pointer
    -> List (H.Attribute msg)
mouseAttributes mouseDetector pointerMaker msgMaker currentTool previousPointer =
    [ mouseDetector "mousedown" <| msgMaker << (pointerMaker Down)
    , mouseDetector "mouseup" <| msgMaker << (pointerMaker Up)
    ]
        ++ if previousPointer == Nothing then
            []
           else
            [ mouseDetector "mousemove" <| msgMaker << (pointerMaker Move) ]


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
