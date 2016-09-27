module Utils.DOM exposing
    ( target, currentTarget, offsetParent
    , offsetWidth, offsetHeight, offsetLeft, offsetTop
    , scrollLeft, scrollTop
    , Rectangle, boundingClientRect
    , offset, client, page, movement
    , computedOffset, test
    )

import Json.Decode as J
import Json.Decode exposing ((:=))


-- DOM DECODERS (from elm-dom package)

{-| Get the target DOM element of an event -}
target : J.Decoder a -> J.Decoder a
target decoder =
    "target" := decoder

{-| Get the offsetParent of the current element.
Returns first argument if the current element is already the root;
applies the second argument to the parent element if not.
To do traversals of the DOM, exploit that Elm allows recursive values.
-}
offsetParent : a -> J.Decoder a -> J.Decoder a
offsetParent x decoder =
    J.oneOf
        [ "offsetParent" := J.null x
        , "offsetParent" := decoder
        ]

{-| Get the width of an element in pixels -}
offsetWidth : J.Decoder Float
offsetWidth = "offsetWidth" := J.float

{-| Get the heigh of an element in pixels -}
offsetHeight : J.Decoder Float
offsetHeight = "offsetHeight" := J.float

{-| Get the left-offset of the element in the parent element in pixels -}
offsetLeft : J.Decoder Float
offsetLeft = "offsetLeft" := J.float

{-| Get the top-offset of the element in the parent element in pixels -}
offsetTop : J.Decoder Float
offsetTop = "offsetTop" := J.float

{-| Get the amount of left scroll of the element in pixels -}
scrollLeft : J.Decoder Float
scrollLeft = "scrollLeft" := J.float

{-| Get the amount of top scroll of the element in pixels -}
scrollTop : J.Decoder Float
scrollTop = "scrollTop" := J.float

{-| Type for rectangles -}
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
boundingClientRect : J.Decoder Rectangle
boundingClientRect =
    J.object3
        (\(x, y) width height -> Rectangle x y width height)
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
position : Float -> Float -> J.Decoder (Float, Float)
position x y =
    J.object4
        (\scrollLeft scrollTop offsetLeft offsetTop ->
            (x + offsetLeft - scrollLeft, y + offsetTop - scrollTop))
        scrollLeft
        scrollTop
        offsetLeft
        offsetTop
    `J.andThen`
    (\(x',y') -> offsetParent (x', y') (position x' y'))


-- DOM DECODERS (my extension)

{-| Get the current target DOM element of an event -}
currentTarget : J.Decoder a -> J.Decoder a
currentTarget decoder =
    "currentTarget" := decoder

{-| Decode the "offsetX and "offsetY" values from a JSON -}
offset : J.Decoder (Float, Float)
offset = J.object2 (,) ("offsetX" := J.float) ("offsetY" := J.float)

{-| Decode the "clientX" and "clientY" values from a JSON -}
client : J.Decoder (Float, Float)
client = J.object2 (,) ("clientX" := J.float) ("clientY" := J.float)

{-| Decode the "pageX" and "pageY" values from a JSON -}
page : J.Decoder (Float, Float)
page = J.object2 (,) ("pageX" := J.float) ("pageY" := J.float)

{-| Decode computed (expensively) offset from values from a JSON.
Should be replaced by offset when implemented correctly in firefox
(with currentTarget instead of target).
-}
computedOffset : J.Decoder (Float, Float)
computedOffset =
    J.object2
        (\(x,y) rect -> (x-rect.left, y-rect.top))
        page
        (currentTarget boundingClientRect)

{-| Decode the "movementX" and "movementY" values from a JSON -}
movement : J.Decoder (Float, Float)
movement = J.object2 (,) ("movementX" := J.float) ("movementY" := J.float)

test : J.Decoder (Float, Float)
test = J.object1 (\rect -> (rect.left, rect.top)) boundingClientRect
