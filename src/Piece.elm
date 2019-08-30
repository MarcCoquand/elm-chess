module Piece exposing
    ( HasMoved
    , Piece(..)
    , isKing
    , isUnmovedKing
    , isUnmovedRook
    , move
    , show
    , update
    , view
    )

import Element exposing (Element)
import Move exposing (Move)
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


isKing : Piece -> Bool
isKing piece =
    case piece of
        King _ ->
            True

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


move :
    { blank : Predicate Position
    , collision : Predicate Position
    , outOfBounds : Predicate Position
    , swapRight : Predicate Position
    , swapLeft : Predicate Position
    , threatened : Predicate Position
    , belongsToPlayer : Predicate Position
    }
    ->
        { from : Position
        , player : Player
        , piece : Piece
        , to : Position
        }
    -> Move
move predicate { from, player, piece, to } =
    case piece of
        Pawn hasMoved ->
            Move.pawn
                { player = player
                , belongsToPlayer = predicate.belongsToPlayer
                , isBlank = predicate.blank
                , collision = predicate.collision
                , outOfBounds = predicate.outOfBounds
                , hasMoved = hasMoved
                , position = from
                }
                to

        Rook _ ->
            Move.rook
                { belongsToPlayer = predicate.belongsToPlayer
                , outOfBounds = predicate.outOfBounds
                , position = from
                , collision = predicate.collision
                }
                to

        King _ ->
            Move.king
                { belongsToPlayer = predicate.belongsToPlayer
                , outOfBounds = predicate.outOfBounds
                , isThreatened = predicate.threatened
                , position = from
                , swapRight = predicate.swapRight
                , swapLeft = predicate.swapLeft
                }
                to

        Queen ->
            Move.queen
                { belongsToPlayer = predicate.belongsToPlayer
                , outOfBounds = predicate.outOfBounds
                , position = from
                , collision = predicate.collision
                }
                to

        Bishop ->
            Move.bishop
                { belongsToPlayer = predicate.belongsToPlayer
                , outOfBounds = predicate.outOfBounds
                , position = from
                , collision = predicate.collision
                }
                to

        Knight ->
            Move.knight
                { belongsToPlayer = predicate.belongsToPlayer
                , outOfBounds = predicate.outOfBounds
                , position = from
                }
                to


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
