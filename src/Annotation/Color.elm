module Annotation.Color
    exposing
        ( palette
        , toStr
        , turquoise
        )

{-| Color management.

@docs toStr, turquoise, palette

-}

import Color exposing (Color)


{-| Get a string describing the color like `rgba(255,0,0,1)`.
-}
toStr : Color -> String
toStr color =
    let
        rgba =
            Color.toRgb color
    in
    String.concat
        [ "rgb("
        , toString rgba.red
        , ","
        , toString rgba.green
        , ","
        , toString rgba.blue
        , ","
        , toString rgba.alpha
        , ")"
        ]



-- PREDEFINED COLORS #################################################


{-| Turquoise.
-}
turquoise : Color
turquoise =
    Color.rgb 65 182 196



-- COLOR PALETTES ####################################################


{-| A color palette print and color-blind friendly.

( beige, green, turquoise, blue, dark blue )

-}
palette : ( Color, Color, Color, Color, Color )
palette =
    ( Color.rgb 255 255 204
    , Color.rgb 161 218 180
    , turquoise
    , Color.rgb 44 127 184
    , Color.rgb 37 52 148
    )
