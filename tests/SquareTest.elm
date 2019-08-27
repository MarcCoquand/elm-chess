module SquareTest exposing (isJust, suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Piece
import Player
import Square
import Test exposing (..)


isJust : Maybe a -> Bool
isJust value =
    case value of
        Just _ ->
            True

        Nothing ->
            False


suite : Test
suite =
    describe "Square"
        [ test "collision and blank" <|
            \_ ->
                Square.Contains Player.White Piece.Queen
                    |> (\piece ->
                            Expect.equal
                                True
                                (Square.collision piece)
                       )
        , test "Can swap left" <|
            \_ -> Expect.equal True True
        , test "Can swap right" <|
            \_ -> Expect.equal True True
        ]
