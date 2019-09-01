module Piece exposing
    ( HasMoved
    , Piece(..)
    , allPossibleMoves
    , isKing
    , isUnmovedKing
    , isUnmovedRook
    , move
    , rule
    , show
    , update
    , view
    )

import Element exposing (Element)
import Element.Font as Font exposing (Font)
import Move exposing (Move)
import Move.Ruleset as Ruleset exposing (Illegal, Ruleset, Valid)
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


type alias BoardPredicates =
    { blank : Predicate Position
    , collision : Predicate Position
    , outOfBounds : Predicate Position
    , swapRight : Predicate Position
    , swapLeft : Predicate Position
    , threatened : Predicate Position
    , belongsToPlayer : Predicate Position
    }


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
    Element.el [ Font.center, Element.centerX, Element.centerY ] (Element.text (show player piece))


move :
    BoardPredicates
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


rule :
    BoardPredicates
    ->
        { position : Position
        , player : Player
        , piece : Piece
        }
    -> Ruleset Valid
rule predicate { position, player, piece } =
    case piece of
        Pawn hasMoved ->
            Ruleset.pawn
                { player = player
                , belongsToPlayer = predicate.belongsToPlayer
                , isBlank = predicate.blank
                , collision = predicate.collision
                , outOfBounds = predicate.outOfBounds
                , hasMoved = hasMoved
                , position = position
                }

        Rook _ ->
            Ruleset.rook
                { belongsToPlayer = predicate.belongsToPlayer
                , outOfBounds = predicate.outOfBounds
                , position = position
                , collision = predicate.collision
                }

        King _ ->
            Ruleset.king
                { belongsToPlayer = predicate.belongsToPlayer
                , outOfBounds = predicate.outOfBounds
                , isThreatened = predicate.threatened
                , position = position
                , swapRight = predicate.swapRight
                , swapLeft = predicate.swapLeft
                }

        Queen ->
            Ruleset.queen
                { belongsToPlayer = predicate.belongsToPlayer
                , outOfBounds = predicate.outOfBounds
                , position = position
                , collision = predicate.collision
                }

        Bishop ->
            Ruleset.bishop
                { belongsToPlayer = predicate.belongsToPlayer
                , outOfBounds = predicate.outOfBounds
                , position = position
                , collision = predicate.collision
                }

        Knight ->
            Ruleset.knight
                { belongsToPlayer = predicate.belongsToPlayer
                , outOfBounds = predicate.outOfBounds
                , position = position
                }


allPossibleMoves : BoardPredicates -> Player -> List ( Position, Piece ) -> Ruleset Valid
allPossibleMoves predicates player pieces =
    pieces
        |> List.map
            (\( position, piece ) ->
                rule predicates
                    { position = position
                    , piece = piece
                    , player = player
                    }
            )
        |> Ruleset.rules


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
