-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module Helpers.Pivot
    exposing
        ( insertSortedWith
        , neighbours
        )

import Pivot exposing (Pivot)


insertSortedLeft : (a -> a -> Order) -> a -> Pivot a -> Pivot a
insertSortedLeft compare a pivot =
    case Pivot.goL pivot of
        Just left ->
            case compare a (Pivot.getC left) of
                LT ->
                    insertSortedLeft compare a left

                _ ->
                    Pivot.addGoL a pivot

        Nothing ->
            Pivot.addGoL a pivot


insertSortedRight : (a -> a -> Order) -> a -> Pivot a -> Pivot a
insertSortedRight compare a pivot =
    case Pivot.goR pivot of
        Just right ->
            case compare a (Pivot.getC right) of
                GT ->
                    insertSortedRight compare a right

                _ ->
                    Pivot.addGoR a pivot

        Nothing ->
            Pivot.addGoR a pivot


{-| Insert an element in a sorted Pivot.
-}
insertSortedWith : (a -> a -> Order) -> a -> Maybe (Pivot a) -> Pivot a
insertSortedWith compare a maybePivot =
    case maybePivot of
        Nothing ->
            Pivot.singleton a

        Just pivot ->
            if compare a (Pivot.getC pivot) == LT then
                insertSortedLeft compare a pivot
            else
                insertSortedRight compare a pivot


neighbours : Pivot a -> ( Maybe a, Maybe a )
neighbours pivot =
    ( pivot |> Pivot.goL |> Maybe.map Pivot.getC
    , pivot |> Pivot.goR |> Maybe.map Pivot.getC
    )
