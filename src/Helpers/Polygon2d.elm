-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module Helpers.Polygon2d
    exposing
        ( intersection
        )

import OpenSolid.Geometry.Types exposing (Polygon2d(..), Point2d(..), LineSegment2d(..))
import OpenSolid.Polygon2d as Polygon2d
import OpenSolid.LineSegment2d as LineSegment2d
import OpenSolid.Vector2d as Vector2d
import Helpers.LineSegment2d as LineSegment2d
import OpenSolid.Point2d as Point2d
import Helpers.List as List
import Pivot exposing (Pivot)
import Helpers.Pivot as Pivot


type Side
    = Start
    | End


type alias Event =
    ( Int, Side, LineSegment2d )


type alias EventAccumulator =
    ( Maybe (Pivot Event), Maybe Point2d )


reorient : LineSegment2d -> LineSegment2d
reorient segment =
    let
        ( start, end ) =
            LineSegment2d.endpoints segment

        ( startCoord, endCoord ) =
            ( Point2d.coordinates start
            , Point2d.coordinates end
            )
    in
        if startCoord <= endCoord then
            LineSegment2d ( start, end )
        else
            LineSegment2d ( end, start )


eventPos : Event -> ( Float, Float )
eventPos ( _, side, segment ) =
    case side of
        Start ->
            Point2d.coordinates (LineSegment2d.startPoint segment)

        End ->
            Point2d.coordinates (LineSegment2d.endPoint segment)


{-| Evaluate if the starting point of the first segment is below (LT)
or above (GT) the second segment.
-}
height : Event -> Event -> Order
height ( _, _, seg1 ) ( _, _, seg2 ) =
    let
        ( start1, start2 ) =
            ( LineSegment2d.startPoint seg1
            , LineSegment2d.startPoint seg2
            )

        ( s21, v2 ) =
            ( Point2d.vectorFrom start2 start1
            , LineSegment2d.vector seg2
            )

        crossProduct =
            Vector2d.crossProduct v2 s21
    in
        compare crossProduct 0


{-| Check if a polygon intersects itself (in which case it is not "simple").
Returns Nothing if no intersection is found,
otherwise returns the first intersection encountered.
-}
intersection : Polygon2d -> Maybe Point2d
intersection polygon =
    let
        insertSegment : Int -> LineSegment2d -> List Event -> List Event
        insertSegment id seg list =
            ( id, Start, seg ) :: ( id, End, seg ) :: list

        edges : List LineSegment2d
        edges =
            Polygon2d.edges polygon

        eventList : List Event
        eventList =
            edges
                |> List.map reorient
                |> List.indexedFoldr insertSegment []
                |> List.sortBy eventPos

        sweepLine : Maybe (Pivot Event)
        sweepLine =
            Nothing

        nbSegments : Int
        nbSegments =
            List.length edges

        cyclicDistance : Int -> Int -> Int
        cyclicDistance a b =
            let
                diff =
                    abs (a - b)
            in
                min diff (nbSegments - diff)

        eventIntersection : Event -> Event -> Maybe Point2d
        eventIntersection ( id1, _, seg1 ) ( id2, _, seg2 ) =
            if cyclicDistance id1 id2 <= 1 then
                Nothing
            else
                case LineSegment2d.relationshipWith seg1 seg2 of
                    LineSegment2d.Intersection point ->
                        Just point

                    _ ->
                        Nothing

        handleEvent : Event -> EventAccumulator -> EventAccumulator
        handleEvent (( id, side, seg ) as event) ( maybePivot, maybePoint ) =
            if maybePoint == Nothing then
                case side of
                    Start ->
                        let
                            pivot =
                                Pivot.insertSortedWith height event maybePivot

                            ( maybeEventA, maybeEventB ) =
                                Pivot.neighbours pivot

                            intersectionA =
                                Maybe.map (eventIntersection event) maybeEventA
                                    |> Maybe.withDefault Nothing

                            intersectionB =
                                Maybe.map (eventIntersection event) maybeEventB
                                    |> Maybe.withDefault Nothing
                        in
                            case ( intersectionA, intersectionB ) of
                                ( Just point, _ ) ->
                                    ( Nothing, Just point )

                                ( _, Just point ) ->
                                    ( Nothing, Just point )

                                _ ->
                                    ( Just pivot, Nothing )

                    End ->
                        let
                            pivot =
                                maybePivot
                                    |> Maybe.withDefault (Pivot.singleton event)
                                    |> Pivot.firstWith (\( i, _, _ ) -> i == id)
                                    |> Maybe.withDefault (Pivot.singleton event)

                            intersectionAB =
                                case Pivot.neighbours pivot of
                                    ( Just eventA, Just eventB ) ->
                                        eventIntersection eventA eventB

                                    _ ->
                                        Nothing

                            newPivot =
                                if intersectionAB == Nothing then
                                    if Pivot.hasL pivot then
                                        Pivot.removeGoL pivot
                                    else
                                        Pivot.removeGoR pivot
                                else
                                    Nothing
                        in
                            ( newPivot, intersectionAB )
            else
                ( Nothing, maybePoint )
    in
        eventList
            |> List.foldr handleEvent ( sweepLine, Nothing )
            |> Tuple.second
