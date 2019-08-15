module Set.Extra exposing (unions)

import Set exposing (Set)


unions : List (Set comparable) -> Set comparable
unions =
    List.foldl Set.union Set.empty
