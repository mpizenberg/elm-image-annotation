module Helpers.List exposing (..)


zip : List a -> List b -> List ( a, b )
zip =
    List.map2 (,)
