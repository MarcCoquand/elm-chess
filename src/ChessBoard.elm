module ChessBoard exposing (ChessBoard, init, select)

import Board exposing (Board)
import Moves exposing (Moves, Valid)
import Piece exposing (Piece(..))
import Player exposing (Player)
import Position exposing (Position)
import Predicate exposing (Predicate)
import Square exposing (Square)


type alias ChessBoard =
    Board Square


findMoves : Board Square -> Position -> Player -> Piece -> Moves Valid
findMoves board position player piece =
    Piece.validMoves
        { blank = Square.check (Board.get board) Square.blank
        , collision = Square.check (Board.get board) Square.collision
        , outOfBounds = Board.inBounds
        , swapRight = Square.canSwapRight (Board.get board) player
        , swapLeft = Square.canSwapLeft (Board.get board) player
        , threatened = threatened board player
        , belongsToPlayer = Square.check (Board.get board) (Square.belongs player)
        }
        position
        player
        piece


select : Board Square -> Player -> Position -> Maybe ( Square, Moves Valid )
select board player position =
    let
        squareMovesPair owner piece =
            ( Square.Contains owner piece
            , findMoves board position owner piece
            )
    in
    Board.get board position
        |> Maybe.andThen (Square.applyIfOwner player squareMovesPair)


threatened : Board Square -> Player -> Position -> Bool
threatened board player position =
    let
        opponent =
            Player.next player

        isInRangeOf enemyMoves =
            Moves.rules enemyMoves
                |> Moves.member position

        omitSquare =
            Maybe.map (\( _, moves ) -> moves)

        opponentsRange =
            List.filterMap (select board opponent >> omitSquare) Board.positions
    in
    isInRangeOf opponentsRange


init : Maybe (Board Square)
init =
    Board.construct Square.setup
