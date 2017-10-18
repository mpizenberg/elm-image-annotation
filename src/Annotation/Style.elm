module Annotation.Style
    exposing
        ( Fill(..)
        , Line(..)
        , Point(..)
        , fillAttribute
        , fillDefault
        , pointDefault
        , strokeAttributes
        , strokeDefault
        )

{-| Styling the annotations.


# Styling points

@docs Point, pointDefault


# Styling lines

@docs Line, strokeDefault, strokeAttributes


# Styling contents (fill)

@docs Fill, fillDefault, fillAttribute

-}

import Annotation.Color as Color
import Color exposing (Color)
import Svg
import Svg.Attributes as SvgA


{-| Styling of a point.

A point is either not visible,
or displayed as a colored disk of a given radius.

-}
type Point
    = NoPoint
    | Disk Float Color


{-| Default style of a point.

An orange disk of radius 3 px.

-}
pointDefault : Point
pointDefault =
    Disk 3 Color.orange


{-| Styling of a line.

A line is either not visible,
or displayed with a stroke of given width and color.

-}
type Line
    = NoLine
    | Stroke Float Color


{-| Default style of a line.

A red stroke of width 2 px.

-}
strokeDefault : Line
strokeDefault =
    Stroke 2 Color.red


{-| Transform a Line style into the corresponding svg attributes.
-}
strokeAttributes : Line -> List (Svg.Attribute msg)
strokeAttributes line =
    case line of
        NoLine ->
            [ SvgA.stroke "none" ]

        Stroke width color ->
            [ SvgA.strokeWidth (toString width)
            , SvgA.stroke (Color.toStr color)
            ]


{-| Styling of a content (fill).

A fill is either not visible, or of a given color.
Opacity is set using the alpha channel of the color.

-}
type Fill
    = NoFill
    | Fill Color


{-| Default style of a content (fill)

A red with opacity at 20%.

-}
fillDefault : Fill
fillDefault =
    Fill (Color.rgba 255 0 0 0.2)


{-| Transform a Fill style into the corresponding svg attribute.
-}
fillAttribute : Fill -> Svg.Attribute msg
fillAttribute fill =
    case fill of
        NoFill ->
            SvgA.fill "none"

        Fill color ->
            SvgA.fill (Color.toStr color)
