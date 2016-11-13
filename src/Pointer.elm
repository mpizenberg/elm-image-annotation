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
    [ HPE.movementOn "mousedown" <| msgMaker << (fromOffset Down)
    , HPE.movementOn "mouseup" <| msgMaker << (fromOffset Up)
    ]
        ++ if previousPointer == Nothing then
            []
           else
            [ HPE.movementOn "mousemove" <| msgMaker << (fromMovement Move) ]


toolAttributes : (Pointer -> msg) -> Maybe Pointer -> List (H.Attribute msg)
toolAttributes msgMaker previousPointer =
    [ HPE.offsetOn "mousedown" <| msgMaker << (fromOffset Down)
    , HPE.offsetOn "mouseup" <| msgMaker << (fromOffset Up)
    ]
        ++ if previousPointer == Nothing then
            []
           else
            [ HPE.offsetOn "mousemove" <| msgMaker << (fromOffset Move) ]


fromOffset : Event -> ( Float, Float ) -> Pointer
fromOffset event ( offsetX, offsetY ) =
    { event = event
    , offsetX = offsetX
    , offsetY = offsetY
    , movementX = 0
    , movementY = 0
    }


fromMovement : Event -> ( Float, Float ) -> Pointer
fromMovement event ( movementX, movementY ) =
    { event = event
    , offsetX = 0
    , offsetY = 0
    , movementX = movementX
    , movementY = movementY
    }
