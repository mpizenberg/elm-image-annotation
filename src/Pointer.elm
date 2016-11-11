module Pointer exposing (..)

import Html as H exposing (Html)
import Html.Events as HE
import Helpers.Events as HPE


type Event
    = Down
    | Move
    | Up
    | Cancel


type alias Pointer =
    { event : Event
    , offsetX : Int
    , offsetY : Int
    }


attributes : (Pointer -> msg) -> Maybe Pointer -> List (H.Attribute msg)
attributes msgMaker previousPointer =
    [ HPE.offsetOn "mousedown" <| msgMaker << (uncurry <| Pointer Down)
    , HPE.offsetOn "mouseup" <| msgMaker << (uncurry <| Pointer Up)
    ]
        ++ if previousPointer == Nothing then
            []
           else
            [ HPE.offsetOn "mousemove" <| msgMaker << (uncurry <| Pointer Move) ]
