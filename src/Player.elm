module Player exposing (Player(..), display, equal, isOpponent, next)

import Element exposing (Element)
import Predicate exposing (Predicate)


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


show : Player -> String
show player =
    case player of
        White ->
            "White"

        Black ->
            "Black"


equal : Player -> Player -> Bool
equal p1 p2 =
    p1 == p2


isOpponent : Player -> Player -> Bool
isOpponent p1 p2 =
    p1 /= p2


display : Player -> Element msg
display player =
    Element.text (show player)
