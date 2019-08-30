module ChessBoard exposing
    ( ChessBoard
    , Selected
    , init
    , makeMove
    , performMove
    , select
    )

import Board exposing (Board)
import Highlight exposing (Highlight)
import Move exposing (Move)
import Piece exposing (Piece(..))
import Player exposing (Player)
import Position exposing (Position)
import Predicate exposing (Predicate)
import Square exposing (Square)


type alias Selected =
    { player : Player
    , piece : Piece
    , move : Position -> Move
    }


type alias ChessBoard =
    Board Square


makeMove :
    ChessBoard
    -> Position
    -> Player
    -> Piece
    -> Position
    -> Move
makeMove board from player piece to =
    Piece.move
        { blank = Square.check (Board.get board) Square.blank
        , collision = Square.check (Board.get board) Square.collision
        , outOfBounds = not << Board.inBounds
        , swapRight = Square.canSwapRight (Board.get board) player
        , swapLeft = Square.canSwapLeft (Board.get board) player
        , threatened =
            \toAttempt ->
                threatened
                    { board = board
                    , player = player
                    , piece =
                        piece
                    , start = from
                    , attempt = toAttempt
                    }
        , belongsToPlayer = Square.check (Board.get board) (Square.belongs player)
        }
        { from = from
        , to = to
        , player = player
        , piece = piece
        }


change :
    List { piece : Piece, player : Player, from : Position, to : Position }
    -> ChessBoard
    -> ChessBoard
change pieces board =
    let
        makeSquare player piece =
            Square.Contains player (Piece.update piece)

        coordinates =
            List.concatMap
                (\{ piece, player, from, to } ->
                    [ ( to, makeSquare player piece )
                    , ( from, Square.Empty )
                    ]
                )
                pieces
    in
    Board.update coordinates board


performMove : ChessBoard -> Move -> Maybe ChessBoard
performMove board move =
    case move of
        Move.Single { from, to } ->
            Board.get board from
                |> Maybe.andThen Square.toMaybe
                |> Maybe.map
                    (\( player, piece ) ->
                        change [ { piece = piece, player = player, from = from, to = to } ]
                            board
                    )

        Move.Swap { from, to, swapFrom, swapTo } ->
            let
                swapPiece =
                    Board.get board swapFrom
                        |> Maybe.andThen Square.toMaybe

                piece =
                    Board.get board from
                        |> Maybe.andThen Square.toMaybe
            in
            Maybe.map2
                (\( swapPlayer, sp ) ( player, p ) ->
                    change
                        [ { piece = p, player = player, from = from, to = to }
                        , { piece = sp
                          , player = swapPlayer
                          , from = swapFrom
                          , to = swapTo
                          }
                        ]
                        board
                )
                swapPiece
                piece

        Move.Invalid ->
            Nothing


simulateMove :
    ChessBoard
    -> Position
    -> Player
    -> Piece
    -> (Position -> Move)
simulateMove board from player piece to =
    Piece.move
        { blank = Square.check (Board.get board) Square.blank
        , collision = Square.check (Board.get board) Square.collision
        , outOfBounds = not << Board.inBounds
        , swapRight = Square.canSwapRight (Board.get board) player
        , swapLeft = Square.canSwapLeft (Board.get board) player
        , threatened = \_ -> False
        , belongsToPlayer = Square.check (Board.get board) (Square.belongs player)
        }
        { from = from
        , player = player
        , piece = piece
        , to = to
        }


select : ChessBoard -> Player -> Position -> Maybe Selected
select board player position =
    let
        selected owner piece =
            makeMove board position owner piece
                |> (\mover ->
                        { player = owner
                        , move = mover
                        , piece = piece
                        }
                   )
    in
    Board.get board position
        |> Maybe.andThen (Square.applyIfOwner player selected)


simulateSelect : ChessBoard -> Player -> Position -> Maybe Selected
simulateSelect board player position =
    let
        selected owner piece =
            simulateMove board position owner piece
                |> (\mover ->
                        { player = owner
                        , move = mover
                        , piece = piece
                        }
                   )
    in
    Board.get board position
        |> Maybe.andThen (Square.applyIfOwner player selected)


threatened : { board : ChessBoard, player : Player, piece : Piece, start : Position, attempt : Position } -> Bool
threatened { board, player, piece, start, attempt } =
    let
        opponent =
            Player.next player

        canMoveTo mover =
            mover attempt
                |> Move.isValid

        opponentsPossibleMoves =
            List.filterMap (simulateSelect board opponent >> Maybe.map .move) Board.positions
    in
    List.all canMoveTo opponentsPossibleMoves


kingPosition : ChessBoard -> Player -> Position -> Bool
kingPosition board player position =
    Square.checkPiece
        (Board.get board)
        ((==) player)
        Piece.isKing
        position



-- victory : ChessBoard -> Player -> Bool
-- victory board player =
-- let
-- opponent =
-- Player.next player
-- range =
-- -- This is an indicator that we have the wrong abstraction, it's
-- -- impossible that the king does not exist!
-- List.filterMap (select board opponent >> Maybe.map .moves) Board.positions
-- maybeKingPosition =
-- Board.positions
-- |> List.filter (kingPosition board player)
-- |> List.head
-- kingMoves piece =
-- Maybe.map
-- (\position ->
-- findMoves board position opponent piece
-- )
-- in
-- case maybeKingPosition of
-- Just kp ->
-- Moves.contains kp range
-- Nothing ->
-- False


init : Maybe ChessBoard
init =
    Square.placeAll
        |> Board.construct
