-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module Annotation
    exposing
        ( Annotation
        , Input
        , ScribbleType
        , empty
          -- Update
        , setLabel
        , setRectangle
        , initOutline
        , initScribble
        , addPoint
        , update
          -- View
        , view
        , Option
        , optionDescriber
          -- Exports
        , encodePath
          -- Other
        , Check(..)
        , isValid
        , isValidWithGT
        , areValidScribbles
        , areValidScribblesWithGT
        )

{-| An annotation can be a selection or a scribble.

# Model
@docs Annotation, Input, ScribbleType, empty

# Update
@docs setLabel, setRectangle, initOutline, initScribble, addPoint, update

# View
@docs view, Option, optionDescriber

# Exports
@docs encodePath

# Other
@docs Check, isValid, isValidWithGT, areValidScribbles, areValidScribblesWithGT
-}

import Svg exposing (Svg)
import Svg.Attributes as Attributes
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Point2d as Point2d
import OpenSolid.Polygon2d as Polygon2d
import Helpers.Polygon2d as Polygon2d
import OpenSolid.Polyline2d as Polyline2d
import OpenSolid.BoundingBox2d as BoundingBox2d
import OpenSolid.Svg as Svg
import Json.Encode as Encode
import OpenSolid.Geometry.Encode as Encode
import Pointer exposing (Pointer)
import Tool exposing (Tool)
import RLE exposing (RLE)
import Matrix exposing (Matrix)
import Array.Hamt as Array exposing (Array)


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
    | Scribble ScribbleType Polyline2d


{-| The type of scribble (FG is ForeGround, BG is BackGround).
-}
type ScribbleType
    = FG
    | BG


{-| An empty annotation, only useful as startup.
-}
empty : String -> Annotation
empty label =
    { label = label
    , input = Selection <| Polygon2d []
    }



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
initScribble : ScribbleType -> Point2d -> Annotation -> Annotation
initScribble type_ origin annotation =
    { annotation | input = Scribble type_ (Polyline2d [ origin ]) }


{-| Add a point to the input
-}
addPoint : Point2d -> Annotation -> Annotation
addPoint point annotation =
    let
        input =
            case annotation.input of
                Selection (Polygon2d pointsList) ->
                    Selection (Polygon2d (point :: pointsList))

                Scribble type_ (Polyline2d pointsList) ->
                    Scribble type_ (Polyline2d (point :: pointsList))
    in
        { annotation | input = input }


{-| Update an annotation depending on pointer events
-}
update : Pointer -> Pointer.Track -> Tool -> Int -> Option -> Option
update pointer track tool newId option =
    case ( tool, track, pointer.event, option ) of
        ( Tool.Rectangle, _, Pointer.Down, _ ) ->
            updateRectangle pointer pointer newId option

        ( Tool.Rectangle, Pointer.Started start, Pointer.Move, _ ) ->
            updateRectangle start pointer newId option

        ( Tool.Rectangle, Pointer.Moved start _, Pointer.Move, _ ) ->
            updateRectangle start pointer newId option

        ( Tool.Outline, _, Pointer.Down, _ ) ->
            Maybe.withDefault ( newId, empty "outline" ) option
                |> Tuple.mapSecond (initOutline (Point2d <| Pointer.offset pointer))
                |> Just

        ( Tool.Outline, _, Pointer.Move, Just ( id, annotation ) ) ->
            Just ( id, addPoint (Point2d <| Pointer.offset pointer) annotation )

        ( Tool.ScribbleFG, _, Pointer.Down, _ ) ->
            Maybe.withDefault ( newId, empty "scribble" ) option
                |> Tuple.mapSecond (initScribble FG (Point2d <| Pointer.offset pointer))
                |> Just

        ( Tool.ScribbleBG, _, Pointer.Down, _ ) ->
            Maybe.withDefault ( newId, empty "scribble" ) option
                |> Tuple.mapSecond (initScribble BG (Point2d <| Pointer.offset pointer))
                |> Just

        ( Tool.ScribbleFG, _, Pointer.Move, Just ( id, annotation ) ) ->
            Just ( id, addPoint (Point2d <| Pointer.offset pointer) annotation )

        ( Tool.ScribbleBG, _, Pointer.Move, Just ( id, annotation ) ) ->
            Just ( id, addPoint (Point2d <| Pointer.offset pointer) annotation )

        _ ->
            option


updateRectangle : Pointer -> Pointer -> Int -> Option -> Option
updateRectangle startPointer endPointer newId option =
    let
        startPoint =
            Point2d (Pointer.offset startPointer)

        endPoint =
            Point2d (Pointer.offset endPointer)
    in
        Maybe.withDefault ( newId, empty "rectangle" ) option
            |> Tuple.mapSecond (setRectangle startPoint endPoint)
            |> Just



-- VIEW ##############################################################


{-| View an annotation input.
-}
view : Annotation -> Svg msg
view annotation =
    let
        red =
            Attributes.stroke "red"

        green =
            Attributes.stroke "green"

        defaultStyle =
            [ Attributes.fillOpacity "0"
            , Attributes.strokeWidth "2"
            , Attributes.pointerEvents "none"
            ]
    in
        case annotation.input of
            Scribble FG _ ->
                viewWithStyle (green :: defaultStyle) annotation

            _ ->
                viewWithStyle (red :: defaultStyle) annotation


{-| View an annotation input with a specific style.
-}
viewWithStyle : List (Svg.Attribute msg) -> Annotation -> Svg msg
viewWithStyle attributes annotation =
    case annotation.input of
        Selection polygon ->
            Svg.polygon2d attributes polygon

        Scribble type_ polyline ->
            Svg.polyline2d attributes polyline


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

                        Scribble FG _ ->
                            "FG Scribble"

                        Scribble BG _ ->
                            "BG Scribble"
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

        Scribble FG polyline ->
            Encode.object
                [ ( "type", Encode.string "FG" )
                , ( "path", Encode.polyline2d polyline )
                ]

        Scribble BG polyline ->
            Encode.object
                [ ( "type", Encode.string "BG" )
                , ( "path", Encode.polyline2d polyline )
                ]



-- OTHER #############################################################


{-| Indicate if an annotation is valid.
-}
type Check
    = Valid
    | SegmentsCrossing Point2d
    | AreaUnderLimit Float
    | CrossingGT (Matrix Bool)
    | FGLengthToShort
    | BGLengthToShort
    | FGScribbleOutsideGT BoundingBox2d
    | BGScribbleInsideGT BoundingBox2d


andCheck : (() -> Check) -> Check -> Check
andCheck newCheck previousCheck =
    if previousCheck == Valid then
        newCheck ()
    else
        previousCheck


checkIntersection : Polygon2d -> Check
checkIntersection polygon =
    case Polygon2d.intersection polygon of
        Nothing ->
            Valid

        Just point ->
            SegmentsCrossing point


checkAreaOver : Float -> Polygon2d -> Check
checkAreaOver limit polygon =
    if Polygon2d.area polygon >= limit then
        Valid
    else
        AreaUnderLimit limit


checkCrossing : RLE -> Polygon2d -> Check
checkCrossing groundtruth polygon =
    let
        matrixIntersection =
            RLE.fromPolygon 0 0 groundtruth.width groundtruth.height polygon
                |> RLE.toMatrix
                |> Matrix.map2 (\gt poly -> gt && (not poly)) (RLE.toMatrix groundtruth)
    in
        case matrixIntersection of
            Just matrix ->
                if Array.foldr (||) False matrix.data then
                    CrossingGT matrix
                else
                    Valid

            _ ->
                Valid


{-| Indicates if the input of an annotation is valid.
-}
isValid : Annotation -> Check
isValid annotation =
    case annotation.input of
        Selection polygon ->
            checkIntersection polygon
                |> andCheck (\() -> checkAreaOver 500 polygon)

        _ ->
            Valid


{-| Indicates if the input of an annotation is valid using a groundtruth RLE.
-}
isValidWithGT : RLE -> Annotation -> Check
isValidWithGT groundtruth annotation =
    case annotation.input of
        Selection polygon ->
            isValid annotation
                |> andCheck (\() -> checkCrossing groundtruth polygon)

        _ ->
            Valid


{-| Indicates if a list of scribbles is valid.
-}
areValidScribbles : Float -> Float -> List Annotation -> Check
areValidScribbles fgLimit bgLimit annotations =
    let
        ( bgScribbles, fgScribbles ) =
            ( List.filterMap whenScribbleBG annotations
            , List.filterMap whenScribbleFG annotations
            )

        ( bgLength, fgLength ) =
            ( List.sum (List.map Polyline2d.length bgScribbles)
            , List.sum (List.map Polyline2d.length fgScribbles)
            )
    in
        if fgLength < fgLimit then
            FGLengthToShort
        else if bgLength < bgLimit then
            BGLengthToShort
        else
            Valid


{-| Indicates if a list of scribbles is valid (check groundtruth).
-}
areValidScribblesWithGT : RLE -> Float -> Float -> List Annotation -> Check
areValidScribblesWithGT groundtruth fgLimit bgLimit annotations =
    let
        ( bgLines, fgLines ) =
            ( List.filterMap whenScribbleBG annotations
            , List.filterMap whenScribbleFG annotations
            )

        gtMatrix =
            RLE.toMatrix groundtruth
    in
        checkBGScribbleInsideGT gtMatrix bgLines
            |> andCheck (\() -> checkFGScribbleOutsideGT gtMatrix fgLines)
            |> andCheck (\() -> areValidScribbles fgLimit bgLimit annotations)


checkBGScribbleInsideGT : Matrix Bool -> List Polyline2d -> Check
checkBGScribbleInsideGT groundtruth bgLines =
    List.map Polyline2d.vertices bgLines
        |> List.concat
        |> insidePoints groundtruth
        |> BoundingBox2d.containing
        |> Maybe.map BGScribbleInsideGT
        |> Maybe.withDefault Valid


checkFGScribbleOutsideGT : Matrix Bool -> List Polyline2d -> Check
checkFGScribbleOutsideGT groundtruth fgLines =
    List.map Polyline2d.vertices fgLines
        |> List.concat
        |> outsidePoints groundtruth
        |> BoundingBox2d.containing
        |> Maybe.map FGScribbleOutsideGT
        |> Maybe.withDefault Valid



-- HELPERS ###########################################################


whenScribbleBG : Annotation -> Maybe Polyline2d
whenScribbleBG ann =
    case ann.input of
        Scribble BG polyline ->
            Just polyline

        _ ->
            Nothing


whenScribbleFG : Annotation -> Maybe Polyline2d
whenScribbleFG ann =
    case ann.input of
        Scribble FG polyline ->
            Just polyline

        _ ->
            Nothing


insidePoints : Matrix Bool -> List Point2d -> List Point2d
insidePoints gt points =
    let
        consIfInsideGT : Point2d -> List Point2d -> List Point2d
        consIfInsideGT point accumulator =
            case getAt (Point2d.coordinates point |> mapBoth round) gt of
                Just True ->
                    point :: accumulator

                _ ->
                    accumulator
    in
        List.foldl consIfInsideGT [] points


outsidePoints : Matrix Bool -> List Point2d -> List Point2d
outsidePoints gt points =
    let
        consIfOutsideGT : Point2d -> List Point2d -> List Point2d
        consIfOutsideGT point accumulator =
            case getAt (Point2d.coordinates point |> mapBoth round) gt of
                Just True ->
                    accumulator

                _ ->
                    point :: accumulator
    in
        List.foldl consIfOutsideGT [] points


getAt : ( Int, Int ) -> Matrix a -> Maybe a
getAt =
    uncurry Matrix.get


mapBoth : (a -> b) -> ( a, a ) -> ( b, b )
mapBoth f ( x, y ) =
    ( f x, f y )
