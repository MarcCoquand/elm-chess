module SquareTest exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Piece exposing (Piece(..))
import Player
import Position exposing (Position)
import Square exposing (Square)
import Test exposing (..)


swapMockShouldFail : Position -> Maybe Square
swapMockShouldFail ( x, y ) =
    case ( x, y ) of
        ( 0, 0 ) ->
            Square.Contains Player.White (Rook False)
                |> Just

        ( 1, 0 ) ->
            Square.Contains Player.White (Rook False)
                |> Just

        ( 2, 0 ) ->
            Square.Contains Player.White (Rook False)
                |> Just

        ( 3, 0 ) ->
            Square.Contains Player.White (King False)
                |> Just

        ( 4, 0 ) ->
            Square.Contains Player.White (Rook False)
                |> Just

        ( 5, 0 ) ->
            Square.Contains Player.White (Rook False)
                |> Just

        ( 6, 0 ) ->
            Square.Contains Player.White (Rook False)
                |> Just

        ( 7, 0 ) ->
            Square.Contains Player.White (Rook False)
                |> Just

        _ ->
            Nothing


swapMock : Position -> Maybe Square
swapMock ( x, y ) =
    case ( x, y ) of
        ( 0, 0 ) ->
            Square.Contains Player.White (Rook False)
                |> Just

        ( 1, 0 ) ->
            Square.Empty
                |> Just

        ( 2, 0 ) ->
            Square.Empty
                |> Just

        ( 3, 0 ) ->
            Square.Contains Player.White (King False)
                |> Just

        ( 4, 0 ) ->
            Square.Empty
                |> Just

        ( 5, 0 ) ->
            Square.Empty
                |> Just

        ( 6, 0 ) ->
            Square.Empty
                |> Just

        ( 7, 0 ) ->
            Square.Contains Player.White (Rook False)
                |> Just

        _ ->
            Nothing


suite : Test
suite =
    describe "Square"
        [ test "collision and blank" <|
            \_ ->
                Square.Contains Player.White Piece.Queen
                    |> Square.collision
                    |> Expect.equal True
        , test "Can swap left" <|
            \_ ->
                Square.canSwapLeft swapMock Player.White ( 3, 0 )
                    |> Expect.equal True
        , test "Can swap right" <|
            \_ ->
                Square.canSwapRight swapMock Player.White ( 3, 0 )
                    |> Expect.equal True
        , test "Can not swap left if obsticle" <|
            \_ ->
                Square.canSwapLeft swapMockShouldFail Player.White ( 3, 0 )
                    |> Expect.equal False
        , test "Can not swap right if obsticle" <|
            \_ ->
                Square.canSwapRight swapMockShouldFail Player.White ( 3, 0 )
                    |> Expect.equal False
        ]
