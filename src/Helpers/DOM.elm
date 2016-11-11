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
        , test
        )

import Json.Decode as JD exposing ((:=))


-- DOM DECODERS (from elm-dom package)


{-| Get the target DOM element of an event
-}
target : JD.Decoder a -> JD.Decoder a
target decoder =
    "target" := decoder


{-| Get the offsetParent of the current element.
Returns first argument if the current element is already the root;
applies the second argument to the parent element if not.
To do traversals of the DOM, exploit that Elm allows recursive values.
-}
offsetParent : a -> JD.Decoder a -> JD.Decoder a
offsetParent x decoder =
    JD.oneOf
        [ "offsetParent" := JD.null x
        , "offsetParent" := decoder
        ]


{-| Get the width of an element in pixels
-}
offsetWidth : JD.Decoder Float
offsetWidth =
    "offsetWidth" := JD.float


{-| Get the heigh of an element in pixels
-}
offsetHeight : JD.Decoder Float
offsetHeight =
    "offsetHeight" := JD.float


{-| Get the left-offset of the element in the parent element in pixels
-}
offsetLeft : JD.Decoder Float
offsetLeft =
    "offsetLeft" := JD.float


{-| Get the top-offset of the element in the parent element in pixels
-}
offsetTop : JD.Decoder Float
offsetTop =
    "offsetTop" := JD.float


{-| Get the amount of left scroll of the element in pixels
-}
scrollLeft : JD.Decoder Float
scrollLeft =
    "scrollLeft" := JD.float


{-| Get the amount of top scroll of the element in pixels
-}
scrollTop : JD.Decoder Float
scrollTop =
    "scrollTop" := JD.float


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
boundingClientRect : JD.Decoder Rectangle
boundingClientRect =
    JD.object3
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


position : Float -> Float -> JD.Decoder ( Float, Float )
position x y =
    JD.object4
        (\scrollLeft scrollTop offsetLeft offsetTop ->
            ( x + offsetLeft - scrollLeft, y + offsetTop - scrollTop )
        )
        scrollLeft
        scrollTop
        offsetLeft
        offsetTop
        `JD.andThen` (\( x', y' ) -> offsetParent ( x', y' ) (position x' y'))



-- DOM DECODERS (my extension)


{-| Get the current target DOM element of an event
-}
currentTarget : JD.Decoder a -> JD.Decoder a
currentTarget decoder =
    "currentTarget" := decoder


{-| Decode the "offsetX and "offsetY" values from a JSON
-}
offset : JD.Decoder ( Float, Float )
offset =
    JD.object2 (,) ("offsetX" := JD.float) ("offsetY" := JD.float)


{-| Decode the "clientX" and "clientY" values from a JSON
-}
client : JD.Decoder ( Float, Float )
client =
    JD.object2 (,) ("clientX" := JD.float) ("clientY" := JD.float)


{-| Decode the "pageX" and "pageY" values from a JSON
-}
page : JD.Decoder ( Float, Float )
page =
    JD.object2 (,) ("pageX" := JD.float) ("pageY" := JD.float)


{-| Decode computed (expensively) offset from values from a JSON.
Should be replaced by offset when implemented correctly in firefox
(with currentTarget instead of target).
-}
computedOffset : JD.Decoder ( Float, Float )
computedOffset =
    JD.object2
        (\( x, y ) rect -> ( x - rect.left, y - rect.top ))
        page
        (currentTarget boundingClientRect)


{-| Decode the "movementX" and "movementY" values from a JSON
-}
movement : JD.Decoder ( Float, Float )
movement =
    JD.object2 (,) ("movementX" := JD.float) ("movementY" := JD.float)


test : JD.Decoder ( Float, Float )
test =
    JD.object1 (\rect -> ( rect.left, rect.top )) boundingClientRect
