module Utils.Helpers exposing
    (..)


import Html as H
import Html.Events as HE
import Json.Decode as J
import Task


import Utils.DOM as DOM




{-| Transform a message to a Cmd message -}
msgToCmd : msg -> Cmd msg
msgToCmd message =
    Task.perform identity identity (Task.succeed message)




-- ATTRIBUTE MSG ON MOUSE EVENTS #####################################




floatToInt : (Float, Float) -> (Int, Int)
floatToInt (x,y) = (round x, round y)


{-| Get the offsetX and offsetY properties of a mouse event -}
offsetOn : String -> (((Int,Int) -> msg) -> H.Attribute msg)
offsetOn mouseEvent =
    specialOn mouseEvent DOM.offset floatToInt


{-| Generic function to get results from transformed properties at a mouse event.
It uses a decoder and a function transforming the decoder results.
-}
specialOn : String -> J.Decoder a -> (a -> b) -> ((b -> msg) -> H.Attribute msg)
specialOn mouseEvent decoder transform =
    \tagger ->
        HE.onWithOptions
            mouseEvent
            {stopPropagation=True, preventDefault=True}
            (J.map (tagger << transform) decoder)


onChange : (String -> msg) -> H.Attribute msg
onChange tagger =
    HE.on "change" (J.map tagger HE.targetValue)
