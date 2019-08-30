module Player exposing (Player(..), next, view)

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


view : Player -> Element msg
view player =
    Element.text (show player)
