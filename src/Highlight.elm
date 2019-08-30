module Highlight exposing (Highlight(..), blue, red, setBackground, white)

import Element exposing (Element)
import Element.Background as Background


type Highlight
    = Blue
    | Red
    | None


setBackground color =
    case color of
        Blue ->
            Background.color blue

        Red ->
            Background.color red

        None ->
            Background.color white


white =
    Element.rgb255 255 255 255


blue =
    Element.rgb255 200 200 255


red =
    Element.rgb255 255 200 200
