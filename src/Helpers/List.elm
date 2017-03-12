-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module Helpers.List exposing (..)


zip : List a -> List b -> List ( a, b )
zip =
    List.map2 (,)


{-| Find the first element that satisfies a predicate and return
Just that element. If none match, return Nothing.
    find (\num -> num > 5) [2, 4, 6, 8] == Just 6
-}
find : (a -> Bool) -> List a -> Maybe a
find predicate list =
    case list of
        [] ->
            Nothing

        first :: rest ->
            if predicate first then
                Just first
            else
                find predicate rest


{-| Variant of `foldr` that passes the index of the current element to the step function. `indexedFoldr` is to `List.foldr` as `List.indexedMap` is to `List.map`.
-}
indexedFoldr : (Int -> a -> b -> b) -> b -> List a -> b
indexedFoldr func acc list =
    let
        step x ( i, acc ) =
            ( i - 1, func i x acc )
    in
        Tuple.second (List.foldr step ( List.length list - 1, acc ) list)


foldlUntil : (a -> Bool) -> (a -> b -> b) -> b -> List a -> ( b, List a )
foldlUntil predicate func acc list =
    case list of
        [] ->
            ( acc, [] )

        x :: xs ->
            if predicate x then
                ( acc, xs )
            else
                foldlUntil predicate func (func x acc) xs


insertSortedWith : (a -> a -> Order) -> a -> List a -> List a
insertSortedWith compare a list =
    case list of
        x :: xs ->
            if compare a x == GT then
                x :: insertSortedWith compare a xs
            else
                a :: list

        [] ->
            [ a ]


insertSortedBy : (a -> comparable) -> a -> List a -> List a
insertSortedBy f =
    insertSortedWith (\a b -> compare (f a) (f b))


removeSortedBy : (a -> comparable) -> a -> List a -> List a
removeSortedBy f a list =
    case list of
        [] ->
            []

        x :: xs ->
            let
                ( fa, fx ) =
                    ( f a, f x )
            in
                if fa > fx then
                    x :: removeSortedBy f a xs
                else if fa == fx then
                    xs
                else
                    list
