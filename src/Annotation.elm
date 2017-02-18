-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module Annotation
    exposing
        ( Annotation
        , Input
          -- Update
        , setLabel
        , setRectangle
        , initOutline
        , initScribble
        , addPoint
          -- View
        , view
        , Option
        , optionDescriber
          -- Exports
        , encodePath
          -- Other
        , isValid
        )

{-| An annotation can be a selection or a scribble.

# Model
@docs Annotation, Input

# Update
@docs setLabel, setRectangle, initOutline, initScribble, addPoint

# View
@docs view, Option, optionDescriber

# Exports
@docs encodePath

# Other
@docs isValid
-}

import Html as H exposing (Html)
import Svg.Attributes as SvgA
import Svg exposing (Svg)
import OpenSolid.Geometry.Types exposing (Polygon2d(..), Polyline2d(..), Point2d(..))
import OpenSolid.Point2d as Point2d
import OpenSolid.Svg as Svg
import Json.Encode as Encode
import OpenSolid.Geometry.Encode as Encode


-- MODEL #############################################################


{-| An annotation is a labelled visual input.
-}
type alias Annotation =
    { label : String
    , input : Input
    }


{-| A visual input related to an image.
-}
type Input
    = Selection Polygon2d
    | Scribble Polyline2d



-- UPDATE ############################################################


{-| Set the label of an annotation.
-}
setLabel : String -> Annotation -> Annotation
setLabel label annotation =
    { annotation | label = label }


{-| Set the input to be a rectangle selection.
-}
setRectangle : Point2d -> Point2d -> Annotation -> Annotation
setRectangle p1 p2 annotation =
    { annotation | input = Selection (rectangle p1 p2) }


rectangle : Point2d -> Point2d -> Polygon2d
rectangle p1 p2 =
    let
        ( x1, y1 ) =
            Point2d.coordinates p1

        ( x2, y2 ) =
            Point2d.coordinates p2
    in
        Polygon2d [ p1, Point2d ( x1, y2 ), p2, Point2d ( x2, y1 ) ]


{-| Initialize the input with origin point of the outline.
-}
initOutline : Point2d -> Annotation -> Annotation
initOutline origin annotation =
    { annotation | input = Selection (Polygon2d [ origin ]) }


{-| Initialize the input with origin point of the scribble.
-}
initScribble : Point2d -> Annotation -> Annotation
initScribble origin annotation =
    { annotation | input = Scribble (Polyline2d [ origin ]) }


{-| Add a point to the input
-}
addPoint : Point2d -> Annotation -> Annotation
addPoint point annotation =
    let
        input =
            case annotation.input of
                Selection (Polygon2d pointsList) ->
                    Selection (Polygon2d (point :: pointsList))

                Scribble (Polyline2d pointsList) ->
                    Scribble (Polyline2d (point :: pointsList))
    in
        { annotation | input = input }



-- VIEW ##############################################################


{-| View an annotation input.
-}
view : Annotation -> Svg msg
view annotation =
    let
        defaultStyle =
            [ SvgA.stroke "red"
            , SvgA.strokeWidth "2"
            , SvgA.pointerEvents "none"
            ]
    in
        case annotation.input of
            Selection polygon ->
                Svg.polygon2d defaultStyle polygon

            Scribble polyline ->
                Svg.polyline2d defaultStyle polyline


{-| Option type used for <select> tags
-}
type alias Option =
    Maybe ( Int, Annotation )


{-| Text of an <option> tag describing an annotation.
-}
optionDescriber : Option -> String
optionDescriber option =
    case option of
        Nothing ->
            ""

        Just ( id, annotation ) ->
            let
                inputStr =
                    case annotation.input of
                        Selection _ ->
                            "Selection"

                        Scribble _ ->
                            "Scribble"
            in
                toString id ++ " : " ++ inputStr ++ " : " ++ annotation.label



-- EXPORTS ###########################################################


{-| Encode the path of an annotation
-}
encodePath : Annotation -> Encode.Value
encodePath annotation =
    case annotation.input of
        Selection polygon ->
            Encode.polygon2d polygon

        Scribble polyline ->
            Encode.polyline2d polyline



-- OTHER #############################################################


{-| Indicates if the input of an annotation is valid.
-}
isValid : Annotation -> Bool
isValid annotation =
    False
