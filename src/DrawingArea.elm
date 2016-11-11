module DrawingArea exposing (..)

{-| The DrawingArea module aims at collecting annotations.

@docs DrawingArea, default
@docs view
@docs exportAnnotations, exportSelectionsPaths
@docs hasSelection
-}

import Array exposing (Array)
import Svg exposing (Svg)
import Json.Encode as JE
import Image exposing (Image)
import AnnotationSet as AnnSet exposing (AnnotationSet)
import SvgViewer exposing (SvgViewer)
import Tools exposing (Tool)


-- MODEL #############################################################


{-| A drawing area.
-}
type alias DrawingArea =
    { annotations : AnnotationSet
    , viewer : SvgViewer
    , currentTool : Tool
    }


{-| The default drawing area, no annotation and no tool.
-}
default : DrawingArea
default =
    { annotations = Array.empty
    , viewer = SvgViewer.default
    , currentTool = Tools.None
    }



-- UPDATE ############################################################
-- VIEW ##############################################################


{-| View the svg tag representing the DrawingArea model
-}
view : List (Svg.Attribute msg) -> DrawingArea -> Svg msg
view attributes area =
    SvgViewer.view attributes area.annotations area.viewer



-- OUTPUTS ##############################################################


{-| Export the complete model of the annotation set to a JS object
-}
exportAnnotations : DrawingArea -> JE.Value
exportAnnotations area =
    AnnSet.object area.annotations


{-| Export only the seletions paths to a JS object
-}
exportSelectionsPaths : DrawingArea -> JE.Value
exportSelectionsPaths area =
    AnnSet.pathsObject area.annotations



-- OTHER #############################################################


{-| Indicates if the drawing area has at least one selection
-}
hasSelection : DrawingArea -> Bool
hasSelection area =
    AnnSet.hasSelection area.annotations
