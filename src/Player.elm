module Player exposing (Player(..), next)


type Player
    = Black
    | White


next : Player -> Player
next player =
    case player of
        Black ->
            White

        White ->
            Black
