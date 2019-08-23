module Piece exposing
    ( HasMoved
    , Piece(..)
    , isUnmovedKing
    , isUnmovedRook
    , show
    , update
    , validMoves
    , view
    )

import Element exposing (Element)
import Moves exposing (Moves, Valid)
import Player exposing (Player(..))
import Position exposing (Position)
import Predicate exposing (Predicate)
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


validMoves :
    { blank : Predicate Position
    , collision : Predicate Position
    , outOfBounds : Predicate Position
    , swapRight : Predicate Position
    , swapLeft : Predicate Position
    , threatened : Predicate Position
    , belongsToPlayer : Predicate Position
    }
    -> Position
    -> Player
    -> Piece
    -> Moves Valid
validMoves predicate position player piece =
    case piece of
        Pawn hasMoved ->
            Moves.pawn
                { player = player
                , belongsToPlayer = predicate.belongsToPlayer
                , isBlank = predicate.blank
                , collision = predicate.collision
                , outOfBounds = predicate.outOfBounds
                , hasMoved = hasMoved
                , position = position
                }

        Rook _ ->
            Moves.rook
                { belongsToPlayer = predicate.belongsToPlayer
                , outOfBounds = predicate.outOfBounds
                , position = position
                , collision = predicate.collision
                }

        King _ ->
            Moves.king
                { belongsToPlayer = predicate.belongsToPlayer
                , outOfBounds = predicate.outOfBounds
                , isThreatened = predicate.threatened
                , position = position
                , swapRight = predicate.swapRight
                , swapLeft = predicate.swapLeft
                }

        Queen ->
            Moves.queen
                { belongsToPlayer = predicate.belongsToPlayer
                , outOfBounds = predicate.outOfBounds
                , position = position
                , collision = predicate.collision
                }

        Bishop ->
            Moves.bishop
                { belongsToPlayer = predicate.belongsToPlayer
                , outOfBounds = predicate.outOfBounds
                , position = position
                , collision = predicate.collision
                }

        Knight ->
            Moves.knight
                { belongsToPlayer = predicate.belongsToPlayer
                , outOfBounds = predicate.outOfBounds
                , position = position
                }


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
