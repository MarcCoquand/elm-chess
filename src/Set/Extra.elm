module Set.Extra exposing (concatMap, unions)

import Set exposing (Set)


concatMap : (comparable -> Set comparable2) -> Set comparable -> Set comparable2
concatMap f s =
    Set.foldl (Set.union << f) Set.empty s


unions : List (Set comparable) -> Set comparable
unions =
    List.foldl Set.union Set.empty
