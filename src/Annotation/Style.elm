module Annotation.Style
    exposing
        ( Point(..)
        , pointDefault
        , Line(..)
        , strokeDefault
        , strokeAttributes
        , Fill(..)
        , fillAttributes
        )

{-| Styling the annotations.

@docs Point, pointDefault

@docs Line, strokeDefault, strokeAttributes

@docs Fill, fillAttributes

-}

import Annotation.Color as Color exposing (Color)
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


{-| Default style of a line.
-}
strokeDefault : Line
strokeDefault =
    Stroke 1 Color.red


{-| Get the svg attributes of a line.
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


{-| Color of a fill.

A fill is either not visible, or of a given color and opacity.

-}
type Fill
    = NoFill
    | Fill Color Float


{-| Get the svg attributes of a filling.
-}
fillAttributes : Fill -> List (Svg.Attribute msg)
fillAttributes fill =
    case fill of
        NoFill ->
            [ SvgA.fill "none" ]

        Fill color opacity ->
            [ SvgA.fill (Color.toStr color)
            , SvgA.fillOpacity (toString opacity)
            ]
