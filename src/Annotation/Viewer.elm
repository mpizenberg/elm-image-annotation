module Annotation.Viewer exposing (..)

{-| This module provides functions to manage the viewing area.

@docs Size, Viewer, default, setSize

-}

import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Frame2d as Frame2d


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


{-| Default viewer.
-}
default : Viewer
default =
    { frame = Frame2d.xy
    , size = ( 800, 400 )
    , zoom = 1
    }


{-| Reset size of the viewer.
-}
setSize : ( Float, Float ) -> Viewer -> Viewer
setSize size viewer =
    { viewer | size = size }
