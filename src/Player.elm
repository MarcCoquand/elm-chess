module Player exposing (Player(..), display, next)

import Element exposing (Element)


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


display : Player -> Element msg
display player =
    Element.text (show player)
