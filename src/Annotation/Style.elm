module Annotation.Style
    exposing
        ( Color
        , palette
        , Point
        , Line
        , Fill
        )

{-| Styling the annotations.

@docs Color, palette

@docs Point, Line, Fill

-}


{-| A color type.
-}
type alias Color =
    { r : Int
    , g : Int
    , b : Int
    , a : Int
    }


{-| A color palette print and color-blind friendly.

( beige, green, turquoise, blue, dark blue )

-}
palette : ( Color, Color, Color, Color, Color )
palette =
    ( Color 255 255 204 255
    , Color 161 218 180 255
    , Color 65 182 196 255
    , Color 44 127 184 255
    , Color 37 52 148 255
    )


{-| Styling of a point.

A point is either not visible,
or displayed as a colored disk of a given radius.

-}
type Point
    = NoPoint
    | Disk Float Color


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
