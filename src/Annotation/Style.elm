module Annotation.Style
    exposing
        ( Point(..)
        , pointDefault
        , Line(..)
        , Fill(..)
        )

{-| Styling the annotations.

@docs Point, pointDefault, Line, Fill

-}

import Annotation.Color as Color exposing (Color)


{-| Styling of a point.

A point is either not visible,
or displayed as a colored disk of a given radius.

-}
type Point
    = NoPoint
    | Disk Float Color


{-| Default style of a point.
-}
pointDefault : Point
pointDefault =
    Disk 1 Color.red


{-| Styling of a line.

A line is either not visible,
or displayed with a stroke of given width and color.

-}
type Line
    = NoLine
    | Stroke Float Color


{-| Color of a fill.

A fill is either not visible, or of a given color.

-}
type Fill
    = NoFill
    | Fill Color
