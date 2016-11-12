module DrawingArea exposing (..)

{-| The DrawingArea module aims at collecting annotations.

@docs DrawingArea, default
@docs create, remove, get, useTool, updateArea
@docs view, viewAnnotation, selectAnnotationTag, selectToolTag
@docs exportAnnotations, exportSelectionsPaths
@docs hasSelection
-}

import Array exposing (Array)
import Html exposing (Html)
import Svg exposing (Svg)
import Json.Encode as JE
import Image exposing (Image)
import Annotation as Ann exposing (Annotation)
import AnnotationSet as AnnSet exposing (AnnotationSet)
import SvgViewer exposing (SvgViewer)
import Tools exposing (Tool)
import Pointer exposing (Pointer)


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


{-| Create new annotation.
-}
create : DrawingArea -> DrawingArea
create area =
    { area | annotations = Array.push Ann.default area.annotations }


{-| Remove annotation having a certain id.
-}
remove : Int -> DrawingArea -> DrawingArea
remove id area =
    { area | annotations = AnnSet.remove id area.annotations }


{-| Returns an annotation with its id if it exists.
-}
get : Int -> DrawingArea -> Maybe ( Int, Annotation )
get id area =
    case Array.get id area.annotations of
        Nothing ->
            Nothing

        Just annotation ->
            Just ( id, annotation )


{-| Change the current tool.
-}
useTool : Tool -> DrawingArea -> DrawingArea
useTool tool area =
    { area | currentTool = tool }


{-| Update the drawing area depending on the mouse event.
-}
updateArea : ( Float, Float ) -> Pointer -> Maybe ( Int, Annotation ) -> DrawingArea -> ( Maybe ( Int, Annotation ), DrawingArea )
updateArea origin pointer current area =
    case area.currentTool of
        Tools.None ->
            let
                moveX =
                    pointer.movementX

                moveY =
                    pointer.movementY
            in
                ( current
                , { area | viewer = SvgViewer.move ( moveX, moveY ) area.viewer }
                )

        _ ->
            let
                event =
                    case pointer.event of
                        Pointer.Down ->
                            Ann.Start

                        _ ->
                            Ann.Continue

                ( x, y ) =
                    SvgViewer.transformPos area.viewer ( pointer.offsetX, pointer.offsetY )

                ( ox, oy ) =
                    SvgViewer.transformPos area.viewer origin

                ( newCurrent, newSet ) =
                    AnnSet.update event ( ox, oy ) ( x, y ) area.currentTool current area.annotations
            in
                ( newCurrent
                , { area | annotations = newSet }
                )



-- VIEW ##############################################################


{-| View the svg tag representing the DrawingArea model
-}
view : List (Svg.Attribute msg) -> DrawingArea -> Svg msg
view attributes area =
    SvgViewer.view attributes area.annotations area.viewer


{-| View the given annotation.
-}
viewAnnotation : List (Svg.Attribute msg) -> Maybe Annotation -> DrawingArea -> Svg msg
viewAnnotation attributes maybeAnn area =
    let
        annotation =
            case maybeAnn of
                Nothing ->
                    Array.empty

                Just ann ->
                    Array.fromList [ ann ]
    in
        view attributes { area | annotations = annotation }


{-| Create a <select> tag for the annotations.
-}
selectAnnotationTag :
    DrawingArea
    -> Maybe ( Int, Annotation )
    -> (Maybe ( Int, Annotation ) -> msg)
    -> Html msg
selectAnnotationTag area =
    AnnSet.selectTag area.annotations


{-| Create a <select> tag for the tools.
-}
selectToolTag : DrawingArea -> (Tool -> msg) -> Html msg
selectToolTag area =
    Tools.selectTag area.currentTool



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
