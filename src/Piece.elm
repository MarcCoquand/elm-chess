module Piece exposing (HasMoved, Piece(..), isUnmovedKing, isUnmovedRook, show)

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
