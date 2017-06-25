module Annotation.Viewer exposing (..)

{-| This module provides functions to manage the viewing area.

@docs Size, Viewer

-}

import OpenSolid.Geometry.Types exposing (..)


{-| Size is a type alias for a pair of floats ( width, height ).
-}
type alias Size =
    ( Float, Float )


{-| Parameters of the viewer.
-}
type alias Viewer =
    { frame : Frame2d
    , size : Size
    , zoom : Float
    }
