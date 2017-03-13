-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module RLE
    exposing
        ( RLE
        , toMatrix
        , fromMatrix
        , scanIntersections
        , encodeLine
        , fromPolygon
        )

import Array exposing (Array)
import Array.Extra as Array
import Matrix exposing (Matrix)
import OpenSolid.Geometry.Types exposing (Point2d(..), LineSegment2d(..), Polygon2d(..))
import OpenSolid.LineSegment2d as LineSegment2d
import Helpers.LineSegment2d as LineSegment2d
import OpenSolid.Point2d as Point2d
import OpenSolid.Polygon2d as Polygon2d
import Helpers.Polygon2d as Polygon2d exposing (Event)
import Helpers.List as List


type alias RLE =
    { width : Int
    , height : Int
    , bg_counts : Array Int
    , fg_counts : Array Int
    }


arrayConcat : Array (Array a) -> Array a
arrayConcat =
    Array.foldr Array.append Array.empty


toMatrix : RLE -> Matrix Bool
toMatrix rle =
    { size = ( rle.width, rle.height )
    , data =
        Array.map2
            (\bg fg -> Array.append (Array.repeat bg False) (Array.repeat fg True))
            (rle.bg_counts)
            (rle.fg_counts)
            |> arrayConcat
    }


fromMatrix : Matrix Bool -> RLE
fromMatrix { size, data } =
    let
        processPixel value ( oldValue, bg_count, fg_count, bg_counts, fg_counts ) =
            case ( value, oldValue ) of
                ( False, False ) ->
                    ( value, bg_count + 1, 0, bg_counts, fg_counts )

                ( True, True ) ->
                    ( value, 0, fg_count + 1, bg_counts, fg_counts )

                ( True, False ) ->
                    ( value, 0, 1, bg_count :: bg_counts, fg_counts )

                ( False, True ) ->
                    ( value, 1, 0, bg_counts, fg_count :: fg_counts )

        ( _, bg_count, _, bg_counts, fg_counts ) =
            data
                |> Array.foldr processPixel ( True, 0, 0, [], [] )
    in
        { width = Tuple.first size
        , height = Tuple.second size
        , bg_counts = Array.fromList (bg_count :: bg_counts)
        , fg_counts = Array.fromList fg_counts
        }



-- POLYGON FILLING


type alias EventAccumulator =
    ( List LineSegment2d, List Event )


type alias LineAccumulator =
    ( List Event, List LineSegment2d, List Int, List Int )


eventY : Event -> Float
eventY ( id, side, segment ) =
    case side of
        Polygon2d.Start ->
            LineSegment2d.startPoint segment
                |> Point2d.coordinates
                |> Tuple.second

        Polygon2d.End ->
            LineSegment2d.endPoint segment
                |> Point2d.coordinates
                |> Tuple.second


{-| Reorient a segment by proritizing y coordinate
-}
reorient : LineSegment2d -> LineSegment2d
reorient segment =
    let
        ( start, end ) =
            LineSegment2d.endpoints segment

        ( ( xStart, yStart ), ( xEnd, yEnd ) ) =
            ( Point2d.coordinates start
            , Point2d.coordinates end
            )
    in
        -- switch x and y
        if ( yStart, xStart ) <= ( yEnd, xEnd ) then
            LineSegment2d ( start, end )
        else
            LineSegment2d ( end, start )


fromPolygon : Int -> Int -> Int -> Int -> Polygon2d -> RLE
fromPolygon left top right bottom polygon =
    let
        ( width, height ) =
            ( right - left
            , bottom - top
            )

        insertSegment : Int -> LineSegment2d -> List Event -> List Event
        insertSegment id seg list =
            ( id, Polygon2d.Start, seg ) :: ( id, Polygon2d.End, seg ) :: list

        edges : List LineSegment2d
        edges =
            Polygon2d.edges polygon

        eventList : List Event
        eventList =
            edges
                |> List.map reorient
                |> List.indexedFoldr insertSegment []
                |> List.sortBy eventY

        lines : List Float
        lines =
            List.range top (bottom - 1)
                |> List.map toFloat

        processLine : Float -> LineAccumulator -> LineAccumulator
        processLine y ( events, segments, bg_counts, fg_counts ) =
            let
                ( crossingSegments, followingEvents ) =
                    updateSegments y ( segments, events )

                lineIntersections =
                    scanIntersections y crossingSegments

                ( line_bg_counts, line_fg_counts ) =
                    encodeLine ( toFloat left, toFloat right ) lineIntersections
            in
                ( followingEvents
                , crossingSegments
                , line_bg_counts ++ bg_counts
                , line_fg_counts ++ fg_counts
                )

        ( _, _, bg_counts_per_line, fg_counts_per_line ) =
            List.foldl processLine ( eventList, [], [], [] ) lines

        stick : ( Int, Int ) -> ( List Int, List Int ) -> ( List Int, List Int )
        stick ( bg, fg ) ( bg_acc, fg_acc ) =
            case ( bg_acc, fg_acc ) of
                ( [], _ ) ->
                    ( [ bg ], [ fg ] )

                ( _, [] ) ->
                    ( [ bg ], [ fg ] )

                ( b :: xb, f :: xf ) ->
                    if bg == 0 then
                        ( bg_acc, fg + f :: xf )
                    else if f == 0 then
                        ( bg + b :: xb, fg :: xf )
                    else
                        ( bg :: bg_acc, fg :: fg_acc )

        ( bg_counts, fg_counts ) =
            ( bg_counts_per_line, fg_counts_per_line )
                |> List.foldrPair stick ( [], [] )
    in
        { width = width
        , height = height
        , bg_counts = Array.fromList <| List.reverse bg_counts
        , fg_counts = Array.fromList <| List.reverse fg_counts
        }


updateSegments : Float -> EventAccumulator -> EventAccumulator
updateSegments y ( segments, events ) =
    let
        segmentCoordinates segment =
            ( Point2d.coordinates <| LineSegment2d.startPoint segment
            , Point2d.coordinates <| LineSegment2d.endPoint segment
            )

        processEvent : Event -> List LineSegment2d -> List LineSegment2d
        processEvent ( id, side, segment ) crossingSegments =
            case side of
                Polygon2d.Start ->
                    crossingSegments
                        |> List.insertSortedBy segmentCoordinates segment

                Polygon2d.End ->
                    crossingSegments
                        |> List.removeSortedBy segmentCoordinates segment

        over yLine event =
            yLine < eventY event
    in
        events
            |> List.foldlUntil (over y) processEvent segments


{-| Compute the intersection of the lines given by provided segments and the given x coordinate.
-}
scanIntersections : Float -> List LineSegment2d -> List Float
scanIntersections y segments =
    let
        getPoint relationship =
            case relationship of
                LineSegment2d.Intersection point ->
                    Just point

                LineSegment2d.NonIntersection point ->
                    Just point

                _ ->
                    Nothing

        yLine =
            LineSegment2d ( Point2d ( 0, y ), Point2d ( 1, y ) )
    in
        segments
            |> List.map (LineSegment2d.relationshipWith yLine >> getPoint >> Maybe.map Point2d.xCoordinate)
            |> List.filterMap identity
            |> List.sort


{-| Encode a line provided the ordered intersections coordinates and min-max window.
Carefull since the generated RLE count lists are in reverse order
(from right to left) for efficiency matter.
-}
encodeLine : ( Float, Float ) -> List Float -> ( List Int, List Int )
encodeLine ( yMin, yMax ) scanIntersections =
    let
        ( bg_counts, fg_counts ) =
            encodeLineBG ( yMin, yMax ) scanIntersections ( [], [] )
    in
        if List.length bg_counts == List.length fg_counts then
            ( bg_counts, fg_counts )
        else
            -- bg_counts has one more element than fg_counts
            ( bg_counts, 0 :: fg_counts )


encodeLineBG : ( Float, Float ) -> List Float -> ( List Int, List Int ) -> ( List Int, List Int )
encodeLineBG ( start, end ) scanIntersections ( bg_counts, fg_counts ) =
    case scanIntersections of
        [] ->
            ( round (end - start) :: bg_counts, fg_counts )

        y :: otherIntersections ->
            if y > end then
                ( round (end - start) :: bg_counts, fg_counts )
            else if y > start then
                encodeLineFG
                    ( start + toFloat (round (y - start)), end )
                    otherIntersections
                    ( round (y - start) :: bg_counts, fg_counts )
            else
                encodeLineFG
                    ( start, end )
                    otherIntersections
                    ( 0 :: bg_counts, fg_counts )


encodeLineFG : ( Float, Float ) -> List Float -> ( List Int, List Int ) -> ( List Int, List Int )
encodeLineFG ( start, end ) scanIntersections ( bg_counts, fg_counts ) =
    case scanIntersections of
        [] ->
            ( bg_counts, round (end - start) :: fg_counts )

        y :: otherIntersections ->
            if y > end then
                ( bg_counts, round (end - start) :: fg_counts )
            else if y > start then
                encodeLineBG
                    ( start + toFloat (round (y - start)), end )
                    otherIntersections
                    ( bg_counts, round (y - start) :: fg_counts )
            else
                encodeLineBG
                    ( start, end )
                    otherIntersections
                    ( bg_counts, 0 :: fg_counts )
