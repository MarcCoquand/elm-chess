module Piece exposing
    ( HasMoved
    , Piece(..)
    , isUnmovedKing
    , isUnmovedRook
    , show
    , update
    , view
    )

import Element exposing (Element)
import Player exposing (Player(..))
import Set exposing (Set)


type alias HasMoved =
    Bool


type Piece
    = Pawn HasMoved
    | Rook HasMoved
    | King HasMoved
    | Queen
    | Bishop
    | Knight


isUnmovedRook : Piece -> Bool
isUnmovedRook piece =
    case piece of
        Rook hasMoved ->
            not hasMoved

        _ ->
            False


isUnmovedKing : Piece -> Bool
isUnmovedKing piece =
    case piece of
        King hasMoved ->
            not hasMoved

        _ ->
            False


update : Piece -> Piece
update piece =
    case piece of
        Pawn _ ->
            Pawn True

        Rook _ ->
            Rook True

        King _ ->
            King True

        Queen ->
            Queen

        Bishop ->
            Bishop

        Knight ->
            Knight


view : Player -> Piece -> Element msg
view player piece =
    Element.text (show player piece)


show : Player -> Piece -> String
show player piece =
    case ( player, piece ) of
        ( Black, Pawn _ ) ->
            "♟"

        ( White, Pawn _ ) ->
            "♙"

        ( Black, Knight ) ->
            "♞"

        ( White, Knight ) ->
            "♘"

        ( White, Bishop ) ->
            "♗"

        ( Black, Bishop ) ->
            "♝"

        ( White, Queen ) ->
            "♕"

        ( Black, Queen ) ->
            "♛"

        ( White, Rook _ ) ->
            "♖"

        ( Black, Rook _ ) ->
            "♜"

        ( White, King _ ) ->
            "♔"

        ( Black, King _ ) ->
            "♚"
