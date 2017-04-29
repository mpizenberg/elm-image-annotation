module Annotation.Color
    exposing
        ( Color(..)
        , toStr
        , white
        , black
        , red
        , green
        , blue
        , turquoise
        , palette
        )

{-| Color management.

@docs Color, toStr

@docs white, black, red, green, blue, turquoise

@docs palette

-}


{-| A color type.
-}
type Color
    = RGB Int Int Int


{-| Get a string describing the color like `rgb(255,0,0)`.
-}
toStr : Color -> String
toStr (RGB r g b) =
    [ "rgb(", toString r, ",", toString g, ",", toString b, ")" ]
        |> String.concat



-- PREDEFINED COLORS #################################################


{-| White.
-}
white : Color
white =
    RGB 255 255 255


{-| Black.
-}
black : Color
black =
    RGB 0 0 0


{-| Red.
-}
red : Color
red =
    RGB 255 0 0


{-| Green.
-}
green : Color
green =
    RGB 0 255 0


{-| Blue.
-}
blue : Color
blue =
    RGB 0 0 255


{-| Turquoise.
-}
turquoise : Color
turquoise =
    RGB 65 182 196



-- COLOR PALETTES ####################################################


{-| A color palette print and color-blind friendly.

( beige, green, turquoise, blue, dark blue )

-}
palette : ( Color, Color, Color, Color, Color )
palette =
    ( RGB 255 255 204
    , RGB 161 218 180
    , turquoise
    , RGB 44 127 184
    , RGB 37 52 148
    )
