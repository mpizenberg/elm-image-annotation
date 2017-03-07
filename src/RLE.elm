-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module RLE
    exposing
        ( RLE
        , toMatrix
        , fromMatrix
        , encodeLine
        )

import Array exposing (Array)
import Array.Extra as Array
import Matrix exposing (Matrix)


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


{-| Same as round but halfs are rounded to the lower integer
-}
roundLow : Float -> Int
roundLow =
    negate >> round >> negate


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
            ( roundLow (end - start) :: bg_counts, fg_counts )

        y :: otherIntersections ->
            if y > end then
                ( roundLow (end - start) :: bg_counts, fg_counts )
            else if y > start then
                encodeLineFG ( y, end ) otherIntersections ( roundLow (y - start) :: bg_counts, fg_counts )
            else
                encodeLineFG ( start, end ) otherIntersections ( 0 :: bg_counts, fg_counts )


encodeLineFG : ( Float, Float ) -> List Float -> ( List Int, List Int ) -> ( List Int, List Int )
encodeLineFG ( start, end ) scanIntersections ( bg_counts, fg_counts ) =
    case scanIntersections of
        [] ->
            ( bg_counts, round (end - start) :: fg_counts )

        y :: otherIntersections ->
            if y > end then
                ( bg_counts, round (end - start) :: fg_counts )
            else if y > start then
                encodeLineBG ( y, end ) otherIntersections ( bg_counts, round (y - start) :: fg_counts )
            else
                encodeLineBG ( start, end ) otherIntersections ( bg_counts, 0 :: fg_counts )
