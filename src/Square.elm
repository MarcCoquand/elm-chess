module Square exposing
    ( Square(..)
    , apply
    , applyIfOwner
    , belongs
    , blank
    , blue
    , canSwapLeft
    , canSwapRight
    , check
    , checkPiece
    , collision
    , highlighter
    , isOpponentOf
    , setup
    , updatePiece
    , view
    , white
    )

import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Html exposing (Html)
import Html.Events as Html
import Moves exposing (Moves, Valid)
import Piece exposing (Piece(..))
import Player exposing (Player(..))
import Position exposing (Position)
import Predicate exposing (Predicate)


type Square
    = Empty
    | Contains Player Piece


white =
    Element.rgb255 255 255 255


blue =
    Element.rgb255 200 200 255


highlighter enabled =
    if enabled then
        Background.color blue

    else
        Background.color white


view : { highlight : Bool, square : Square, onClick : msg } -> Element msg
view { highlight, square, onClick } =
    case square of
        Empty ->
            Input.button
                [ highlighter highlight
                , Border.solid
                , Border.width 2
                ]
                { onPress = Just onClick, label = Element.text " " }

        Contains player piece ->
            Input.button [ highlighter highlight, Border.solid, Border.width 2 ]
                { onPress = Just onClick, label = Piece.view player piece }


toMaybe : Square -> Maybe ( Player, Piece )
toMaybe square =
    case square of
        Empty ->
            Nothing

        Contains player piece ->
            Just ( player, piece )


isOpponentOf : Player -> Square -> Bool
isOpponentOf player square =
    case square of
        Empty ->
            False

        Contains owner _ ->
            owner /= player


updatePiece : Square -> Square
updatePiece square =
    case square of
        Empty ->
            Empty

        Contains player piece ->
            Contains player (Piece.update piece)


applyIfOwner : Player -> (Player -> Piece -> a) -> Square -> Maybe a
applyIfOwner player f square =
    case square of
        Contains owner piece ->
            if owner == player then
                Just (f owner piece)

            else
                Nothing

        Empty ->
            Nothing


apply : (Player -> Piece -> a) -> Square -> Maybe a
apply f square =
    case square of
        Contains player piece ->
            Just (f player piece)

        Empty ->
            Nothing



-- PREDICATES


belongs : Player -> Square -> Bool
belongs player square =
    case square of
        Empty ->
            False

        Contains owner _ ->
            player == owner


blank : Square -> Bool
blank square =
    case square of
        Empty ->
            True

        Contains _ _ ->
            False


collision : Square -> Bool
collision square =
    case square of
        Empty ->
            False

        Contains _ _ ->
            True


canSwapRight : (Position -> Maybe Square) -> Player -> Predicate Position
canSwapRight fromBoard player ( x, y ) =
    let
        blankSide =
            List.all (check fromBoard blank) [ ( x + 1, y ), ( x + 2, y ) ]

        isUnmovedRook =
            checkPiece
                fromBoard
                (Player.equal player)
                Piece.isUnmovedRook
                ( x + 3, y )

        isUnmovedKing =
            checkPiece
                fromBoard
                (Player.equal player)
                Piece.isUnmovedKing
                ( x, y )
    in
    blankSide && isUnmovedRook && isUnmovedKing


canSwapLeft : (Position -> Maybe Square) -> Player -> Predicate Position
canSwapLeft getSquare player ( x, y ) =
    let
        blankSide =
            List.all (check getSquare blank) [ ( x - 1, y ), ( x - 2, y ), ( x - 3, y ) ]

        isUnmovedRook =
            checkPiece
                getSquare
                (Player.equal player)
                Piece.isUnmovedRook
                ( x - 4, y )

        isUnmovedKing =
            checkPiece
                getSquare
                (Player.equal player)
                Piece.isUnmovedKing
                ( x, y )
    in
    blankSide && isUnmovedRook && isUnmovedKing


check : (Position -> Maybe Square) -> Predicate Square -> Predicate Position
check getSquare predicate =
    Predicate.withDefault False getSquare predicate


checkPiece :
    (Position -> Maybe Square)
    -> Predicate Player
    -> Predicate Piece
    -> Predicate Position
checkPiece fromBoard playerPredicate piecePredicate =
    let
        squarePredicate =
            Predicate.divideWithDefault False toMaybe playerPredicate piecePredicate
    in
    Predicate.withDefault False fromBoard squarePredicate



-- BOARD


setup : List (List Square)
setup =
    let
        wPa =
            Contains White (Piece.Pawn False)

        wKn =
            Contains White Piece.Knight

        wKi =
            Contains White (Piece.King False)

        wRo =
            Contains White (Piece.Rook False)

        wBi =
            Contains White Piece.Bishop

        wQu =
            Contains White Piece.Queen

        bPa =
            Contains Black (Piece.Pawn False)

        bKn =
            Contains Black Piece.Knight

        bKi =
            Contains Black (Piece.King False)

        bRo =
            Contains Black (Piece.Rook False)

        bBi =
            Contains Black Piece.Bishop

        bQu =
            Contains Black Piece.Queen

        ooo =
            Empty
    in
    [ [ wRo, wKn, wBi, wKi, wQu, wBi, wKn, wRo ]
    , [ wPa, wPa, wPa, wPa, wPa, wPa, wPa, wPa ]
    , [ ooo, ooo, ooo, ooo, ooo, ooo, ooo, ooo ]
    , [ ooo, ooo, ooo, ooo, ooo, ooo, ooo, ooo ]
    , [ ooo, ooo, ooo, ooo, ooo, ooo, ooo, ooo ]
    , [ ooo, ooo, ooo, ooo, ooo, ooo, ooo, ooo ]
    , [ bPa, bPa, bPa, bPa, bPa, bPa, bPa, bPa ]
    , [ bRo, bKn, bBi, bKi, bQu, bBi, bKn, bRo ]
    ]
