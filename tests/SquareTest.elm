module SquareTest exposing (isJust, suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
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
    describe "Ensure Square works"
        [ test "Board is made correctly" <|
            \_ -> Square.initialBoard |> isJust |> Expect.equal True
        ]
