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
    , offsetX : Float
    , offsetY : Float
    , movementX : Float
    , movementY : Float
    }


attributes : (Pointer -> msg) -> Tool -> Maybe Pointer -> List (H.Attribute msg)
attributes msgMaker currentTool previousPointer =
    case currentTool of
        Tools.None ->
            noToolAttributes msgMaker previousPointer

        _ ->
            toolAttributes msgMaker previousPointer


noToolAttributes : (Pointer -> msg) -> Maybe Pointer -> List (H.Attribute msg)
noToolAttributes msgMaker previousPointer =
    [ HPE.movementOn "mousedown" <| msgMaker << (pointerOffset Down)
    , HPE.movementOn "mouseup" <| msgMaker << (pointerOffset Up)
    ]
        ++ if previousPointer == Nothing then
            []
           else
            [ HPE.movementOn "mousemove" <| msgMaker << (pointerMovement Move) ]


toolAttributes : (Pointer -> msg) -> Maybe Pointer -> List (H.Attribute msg)
toolAttributes msgMaker previousPointer =
    [ HPE.offsetOn "mousedown" <| msgMaker << (pointerOffset Down)
    , HPE.offsetOn "mouseup" <| msgMaker << (pointerOffset Up)
    ]
        ++ if previousPointer == Nothing then
            []
           else
            [ HPE.offsetOn "mousemove" <| msgMaker << (pointerOffset Move) ]


pointerOffset : Event -> ( Float, Float ) -> Pointer
pointerOffset event ( offsetX, offsetY ) =
    { event = event
    , offsetX = offsetX
    , offsetY = offsetY
    , movementX = 0
    , movementY = 0
    }


pointerMovement : Event -> ( Float, Float ) -> Pointer
pointerMovement event ( movementX, movementY ) =
    { event = event
    , offsetX = 0
    , offsetY = 0
    , movementX = movementX
    , movementY = movementY
    }
