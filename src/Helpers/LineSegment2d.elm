-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module Helpers.LineSegment2d
    exposing
        ( Relationship(..)
        , relationshipWith
        )

import OpenSolid.Geometry.Types exposing (Point2d(..), LineSegment2d(..))
import OpenSolid.LineSegment2d as LineSegment2d
import OpenSolid.Point2d as Point2d
import OpenSolid.Vector2d as Vector2d


type Relationship
    = Parallel
    | Collinear
    | Intersection Point2d
    | NonIntersection Point2d


relationshipWith : LineSegment2d -> LineSegment2d -> Relationship
relationshipWith segment1 segment2 =
    let
        ( p, q ) =
            ( LineSegment2d.startPoint segment1
            , LineSegment2d.startPoint segment2
            )

        ( r, s, pq ) =
            ( LineSegment2d.vector segment1
            , LineSegment2d.vector segment2
            , Point2d.vectorFrom p q
            )

        ( rXs, pqXr, pqXs ) =
            ( Vector2d.crossProduct r s
            , Vector2d.crossProduct pq r
            , Vector2d.crossProduct pq s
            )
    in
        case ( rXs, pqXr ) of
            ( 0, 0 ) ->
                Collinear

            ( 0, _ ) ->
                Parallel

            _ ->
                let
                    ( t, u ) =
                        ( pqXs / rXs
                        , pqXr / rXs
                        )

                    intersection =
                        LineSegment2d.interpolate segment1 t

                    inside start end x =
                        start <= x && x <= end
                in
                    if inside 0 1 t && inside 0 1 u then
                        Intersection intersection
                    else
                        NonIntersection intersection
