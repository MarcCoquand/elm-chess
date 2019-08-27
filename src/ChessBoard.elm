module ChessBoard exposing (ChessBoard, Selected, init, move, select)

import Board exposing (Board)
import Moves exposing (Moves, Valid)
import Piece exposing (Piece(..))
import Player exposing (Player)
import Position exposing (Position)
import Predicate exposing (Predicate)
import Square exposing (Square)


type alias Selected =
    { position : Position
    , player : Player
    , piece : Piece
    , moves : Moves Valid
    }


type alias ChessBoard =
    Board Square


findMoves : ChessBoard -> Position -> Player -> Piece -> Moves Valid
findMoves board position player piece =
    Piece.validMoves
        { blank = Square.check (Board.get board) Square.blank
        , collision = Square.check (Board.get board) Square.collision
        , outOfBounds = not << Board.inBounds
        , swapRight = Square.canSwapRight (Board.get board) player
        , swapLeft = Square.canSwapLeft (Board.get board) player
        , threatened =
            \to ->
                threatened
                    { board = board
                    , player = player
                    , piece =
                        piece
                    , start = position
                    , attempt = to
                    }
        , belongsToPlayer = Square.check (Board.get board) (Square.belongs player)
        }
        position
        player
        piece


findMovesNoThreat : ChessBoard -> Position -> Player -> Piece -> Moves Valid
findMovesNoThreat board position player piece =
    Piece.validMoves
        { blank = Square.check (Board.get board) Square.blank
        , collision = Square.check (Board.get board) Square.collision
        , outOfBounds = not << Board.inBounds
        , swapRight = Square.canSwapRight (Board.get board) player
        , swapLeft = Square.canSwapLeft (Board.get board) player
        , threatened = \_ -> False
        , belongsToPlayer = Square.check (Board.get board) (Square.belongs player)
        }
        position
        player
        piece


select : ChessBoard -> Player -> Position -> Maybe Selected
select board player position =
    let
        selected owner piece =
            { position = position
            , player = owner
            , moves = findMoves board position owner piece
            , piece = piece
            }
    in
    Board.get board position
        |> Maybe.andThen (Square.applyIfOwner player selected)


simulateSelect : ChessBoard -> Player -> Position -> Maybe Selected
simulateSelect board player position =
    let
        selected owner piece =
            { position = position
            , player = owner
            , moves = findMovesNoThreat board position owner piece
            , piece = piece
            }
    in
    Board.get board position
        |> Maybe.andThen (Square.applyIfOwner player selected)


move :
    List { ownedPiece : ( Player, Piece ), from : Position, to : Position }
    -> ChessBoard
    -> ChessBoard
move pieces board =
    let
        makeSquare ( player, piece ) =
            Square.Contains player (Piece.update piece)

        changes =
            List.concatMap
                (\{ ownedPiece, from, to } ->
                    [ ( to, makeSquare ownedPiece )
                    , ( from, Square.Empty )
                    ]
                )
                pieces
    in
    Board.update changes board


threatened : { board : ChessBoard, player : Player, piece : Piece, start : Position, attempt : Position } -> Bool
threatened { board, player, piece, start, attempt } =
    let
        opponent =
            Player.next player

        simulateBoard =
            move [ { ownedPiece = ( player, piece ), from = start, to = attempt } ]
                board

        opponentsRange =
            List.filterMap (simulateSelect simulateBoard opponent >> Maybe.map .moves) Board.positions
    in
    opponentsRange
        |> Moves.contains attempt


kingPosition : ChessBoard -> Player -> Position -> Bool
kingPosition board player position =
    Square.checkPiece
        (Board.get board)
        ((==) player)
        Piece.isKing
        position


victory : ChessBoard -> Player -> Bool
victory board player =
    let
        opponent =
            Player.next player

        range =
            -- This is an indicator that we have the wrong abstraction, it's
            -- impossible that the king does not exist!
            List.filterMap (select board opponent >> Maybe.map .moves) Board.positions

        maybeKingPosition =
            Board.positions
                |> List.filter (kingPosition board player)
                |> List.head

        kingMoves piece =
            Maybe.map
                (\position ->
                    findMoves board position opponent piece
                )
    in
    case maybeKingPosition of
        Just kp ->
            Moves.contains kp range

        Nothing ->
            False


init : Maybe ChessBoard
init =
    Square.placeAll
        |> Board.construct
