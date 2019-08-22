module ChessBoard exposing (ChessBoard)

import Board exposing (Board)
import Moves exposing (Moves, Valid)
import Piece exposing (Piece(..))
import Player exposing (Player)
import Position exposing (Position)
import Predicate exposing (Predicate)
import Square exposing (Square)



-- Predicates


belongsPredicate : Board Square -> Player -> Predicate Position
belongsPredicate board player =
    Predicate.make (Square.check (belongs player) (Board.get board))


outOfBoundsPredicate : Predicate Position
outOfBoundsPredicate =
    Predicate.make Board.inBounds


collisionPredicate : Board Square -> Predicate Position
collisionPredicate board =
    Predicate.make (Square.check collision (Board.get board))


threatPredicate : Board Square -> Player -> Predicate Position
threatPredicate board player =
    Predicate.make (pieceThreatened board player)


blankPredicate : Board Square -> Predicate Position
blankPredicate board =
    Predicate.make (Square.check Square.blank (Board.get board))


swapRightPredicate : Board Square -> Player -> Predicate Position
swapRightPredicate board player =
    Predicate.make (swapRight (Board.get board) player)


swapLeftPredicate : Board Square -> Player -> Predicate Position
swapLeftPredicate board player =
    Predicate.make (swapLeft (Board.get board) player)


type alias ChessBoard =
    Board Square


movesHelper :
    Board Square
    -> Position
    -> Player
    -> Piece
    -> Moves Valid
movesHelper board position player piece =
    case piece of
        Pawn hasMoved ->
            Moves.pawn
                { player = player
                , belongsToPlayer = belongsPredicate board player
                , isBlank = blankPredicate board
                , collision = collisionPredicate board
                , outOfBounds = outOfBoundsPredicate
                , hasMoved = hasMoved
                , position = position
                }

        Rook _ ->
            Moves.rook
                { belongsToPlayer = belongsPredicate board player
                , outOfBounds = outOfBoundsPredicate
                , position = position
                , collision = collisionPredicate board
                }

        King _ ->
            Moves.king
                { belongsToPlayer = belongsPredicate board player
                , outOfBounds = outOfBoundsPredicate
                , isThreatened = threatPredicate board player
                , position = position
                , swapRight = swapRightPredicate board player
                , swapLeft = swapLeftPredicate board player
                }

        Queen ->
            Moves.queen
                { belongsToPlayer = belongsPredicate board player
                , outOfBounds = outOfBoundsPredicate
                , position = position
                , collision = collisionPredicate board
                }

        Bishop ->
            Moves.bishop
                { belongsToPlayer = belongsPredicate board player
                , outOfBounds = outOfBoundsPredicate
                , position = position
                , collision = collisionPredicate board
                }

        Knight ->
            Moves.knight
                { belongsToPlayer = belongsPredicate board player
                , outOfBounds = outOfBoundsPredicate
                , position = position
                }


moves : Board Square -> Position -> Maybe (Moves Valid)
moves board position =
    Board.get board position
        |> Maybe.andThen get
        |> Maybe.map
            (\( player, piece ) ->
                movesHelper board position player piece
            )


select : Board Square -> Position -> Player -> Maybe ( Square, Moves Valid )
select board position player =
    Board.get board position
        |> Maybe.andThen get
        |> Maybe.map
            (\( owner, piece ) ->
                if player == owner then
                    Just
                        ( Contains owner piece
                        , movesHelper
                            board
                            position
                            player
                            piece
                        )

                else
                    Nothing
            )
        |> Maybe.withDefault Nothing


initialBoard : Maybe (Board Square)
initialBoard =
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
            Blank
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
        |> Board.construct
