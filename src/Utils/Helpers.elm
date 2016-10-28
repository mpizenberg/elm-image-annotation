module Utils.Helpers exposing
    (..)


import Html as H
import Html.Events as HE
import Json.Decode as J


import Utils.DOM as DOM




{-| Update again a couple (model, cmd) with a new message -}
updateAgain
    : (msg -> model -> (model, Cmd msg))
    -> msg
    -> (model, Cmd msg)
    -> (model, Cmd msg)
updateAgain updateFunction msg (model, cmd) =
    let
        (model', cmd') = updateFunction msg model
    in
        model' ! [cmd, cmd']


{-| Update with multiple messages in order -}
updateFull
    : (msg -> model -> (model, Cmd msg))
    -> model
    -> List msg
    -> (model, Cmd msg)
updateFull updateFunction model =
    List.foldl
        (updateAgain updateFunction)
        (model, Cmd.none)




-- ATTRIBUTE MSG ON MOUSE EVENTS #####################################




floatToInt : (Float, Float) -> (Int, Int)
floatToInt (x,y) = (round x, round y)


{-| Get the offsetX and offsetY properties of a mouse event -}
offsetOn : String -> (((Int,Int) -> msg) -> H.Attribute msg)
offsetOn mouseEvent =
    specialOn mouseEvent DOM.offset floatToInt


{-| Get the movementX and movementY properties of a mouse event -}
movementOn : String -> (((Int,Int) -> msg) -> H.Attribute msg)
movementOn mouseEvent =
    specialOn mouseEvent DOM.movement floatToInt


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
