-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module Helpers.DOM
    exposing
        ( target
        , currentTarget
        , offsetParent
        , offsetWidth
        , offsetHeight
        , offsetLeft
        , offsetTop
        , scrollLeft
        , scrollTop
        , Rectangle
        , boundingClientRect
        , offset
        , client
        , page
        , movement
        , computedOffset
        , computedTouchOffset
        )

import Json.Decode as Decode exposing (Decoder)


-- DOM DECODERS (from elm-dom package)


{-| Get the target DOM element of an event
-}
target : Decoder a -> Decoder a
target decoder =
    Decode.field "target" decoder


{-| Get the offsetParent of the current element.
Returns first argument if the current element is already the root;
applies the second argument to the parent element if not.
To do traversals of the DOM, exploit that Elm allows recursive values.
-}
offsetParent : a -> Decoder a -> Decoder a
offsetParent x decoder =
    Decode.oneOf
        [ Decode.field "offsetParent" <| Decode.null x
        , Decode.field "offsetParent" decoder
        ]


{-| Get the width of an element in pixels
-}
offsetWidth : Decoder Float
offsetWidth =
    Decode.field "offsetWidth" Decode.float


{-| Get the heigh of an element in pixels
-}
offsetHeight : Decoder Float
offsetHeight =
    Decode.field "offsetHeight" Decode.float


{-| Get the left-offset of the element in the parent element in pixels
-}
offsetLeft : Decoder Float
offsetLeft =
    Decode.field "offsetLeft" Decode.float


{-| Get the top-offset of the element in the parent element in pixels
-}
offsetTop : Decoder Float
offsetTop =
    Decode.field "offsetTop" Decode.float


{-| Get the amount of left scroll of the element in pixels
-}
scrollLeft : Decoder Float
scrollLeft =
    Decode.field "scrollLeft" Decode.float


{-| Get the amount of top scroll of the element in pixels
-}
scrollTop : Decoder Float
scrollTop =
    Decode.field "scrollTop" Decode.float


{-| Type for rectangles
-}
type alias Rectangle =
    { left : Float
    , top : Float
    , width : Float
    , height : Float
    }


{-| Approximation of the method from the elm-dom package
[getBoundingClientRect](https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XPCOM/Reference/Floaterface/nsIDOMClientRect),
based off
[this stackoverflow answer](https://stackoverflow.com/questions/442404/retrieve-the-position-x-y-of-an-html-element).
NB! This decoder is likely computationally expensive and may produce results
that differ slightly from `getBoundingClientRect` in browser-dependent ways.
Complexity : O(lg n) traversal of the DOM,
only now through presumably expensive JSON decoders.
It's 2007 forever, baby!
-}
boundingClientRect : Decoder Rectangle
boundingClientRect =
    Decode.map3
        (\( x, y ) width height -> Rectangle x y width height)
        (position 0 0)
        offsetWidth
        offsetHeight



{- This is what we're implementing (from the above link).
   function getOffset( el ) {
       var _x = 0;
       var _y = 0;
       while( el && !isNaN( el.offsetLeft ) && !isNaN( el.offsetTop ) ) {
           _x += el.offsetLeft - el.scrollLeft;
           _y += el.offsetTop - el.scrollTop;
           el = el.offsetParent;
       }
       return { top: _y, left: _x };
   }
   var x = getOffset( document.getElementById('yourElId') ).left; )
-}


position : Float -> Float -> Decoder ( Float, Float )
position x y =
    Decode.map4
        (\scrollLeft scrollTop offsetLeft offsetTop ->
            ( x + offsetLeft - scrollLeft, y + offsetTop - scrollTop )
        )
        scrollLeft
        scrollTop
        offsetLeft
        offsetTop
        |> Decode.andThen (\( x_, y_ ) -> offsetParent ( x_, y_ ) (position x_ y_))



-- DOM DECODERS (my extension)


{-| Get the current target DOM element of an event
-}
currentTarget : Decoder a -> Decoder a
currentTarget decoder =
    Decode.field "currentTarget" decoder


{-| Decode the "offsetX and "offsetY" values from a JSON
-}
offset : Decoder ( Float, Float )
offset =
    Decode.map2 (,)
        (Decode.field "offsetX" Decode.float)
        (Decode.field "offsetY" Decode.float)


{-| Decode the "clientX" and "clientY" values from a JSON
-}
client : Decoder ( Float, Float )
client =
    Decode.map2 (,)
        (Decode.field "clientX" Decode.float)
        (Decode.field "clientY" Decode.float)


{-| Decode the "pageX" and "pageY" values from a JSON
-}
page : Decoder ( Float, Float )
page =
    Decode.map2 (,)
        (Decode.field "pageX" Decode.float)
        (Decode.field "pageY" Decode.float)


{-| Decode computed (expensively) offset from values from a JSON.
Should be replaced by offset when implemented correctly in firefox
(with currentTarget instead of target).
-}
computedOffset : Decoder ( Float, Float )
computedOffset =
    Decode.map2
        (\( x, y ) rect -> ( x - rect.left, y - rect.top ))
        page
        (currentTarget boundingClientRect)


{-| Decode the pseudo computed "offsetX" and "offsetY"
-}
computedTouchOffset : Decoder ( Float, Float )
computedTouchOffset =
    Decode.map2
        (\( x, y ) ( left, top ) -> ( x - left, y - top ))
        (Decode.at [ "changedTouches", "0" ] page)
        (currentTarget <| position 0 0)


{-| Decode the "movementX" and "movementY" values from a JSON
-}
movement : Decoder ( Float, Float )
movement =
    Decode.map2 (,)
        (Decode.field "movementX" Decode.float)
        (Decode.field "movementY" Decode.float)
