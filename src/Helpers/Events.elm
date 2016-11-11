module Helpers.Events
    exposing
        ( preventAndStop
        , offsetOn
        , movementOn
        , on
        , onChange
        )

import Html as H exposing (Html)
import Html.Events as HE
import Json.Decode as JD
import Helpers.DOM as DOM


floatToInt : ( Float, Float ) -> ( Int, Int )
floatToInt ( x, y ) =
    ( round x, round y )


preventAndStop : HE.Options
preventAndStop =
    { stopPropagation = True
    , preventDefault = True
    }


{-| Get the offsetX and offsetY properties of a mouse event
-}
offsetOn : String -> (( Float, Float ) -> msg) -> H.Attribute msg
offsetOn mouseEvent msgMaker =
    on mouseEvent DOM.offset msgMaker


{-| Get the movementX and movementY properties of a mouse event
-}
movementOn : String -> (( Float, Float ) -> msg) -> H.Attribute msg
movementOn mouseEvent msgMaker =
    on mouseEvent DOM.movement msgMaker


{-| Generic function to get results from transformed properties at a mouse event.
It uses a decoder and a function transforming the decoder results.
-}
on : String -> JD.Decoder a -> (a -> msg) -> H.Attribute msg
on mouseEvent decoder msgMaker =
    HE.onWithOptions
        mouseEvent
        preventAndStop
        (JD.map msgMaker decoder)


onChange : (String -> msg) -> H.Attribute msg
onChange msgMaker =
    HE.on "change" (JD.map msgMaker HE.targetValue)
