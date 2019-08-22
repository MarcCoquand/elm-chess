module PositionTest exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Position exposing (Position)
import Predicate exposing (Predicate)
import Set exposing (Set)
import Test exposing (..)


position : Fuzzer Position
position =
    Fuzz.tuple ( Fuzz.intRange 0 300, Fuzz.intRange 0 300 )


positionSet : Fuzzer (Set Position)
positionSet =
    Fuzz.list position
        |> Fuzz.map Set.fromList


fakeSet =
    Position.makeMany
        [ { x = 1, y = 1 }
        , { x = 2, y = 2 }
        ]


isCollision : Predicate Position
isCollision =
    Predicate.make
        (\pos ->
            Set.member pos fakeSet
        )


suite : Test
suite =
    describe "Position"
        [ test "Find closest" <|
            \_ ->
                Position.isClosest
                    { isCollision = isCollision
                    , start = Position.make { x = 0, y = 0 }
                    , end = Position.make { x = 1, y = 1 }
                    }
                    |> Expect.equal True
        , test "Does not accept furthest" <|
            \_ ->
                Position.isClosest
                    { isCollision = isCollision
                    , start = Position.make { x = 1, y = 0 }
                    , end = Position.make { x = 2, y = 2 }
                    }
                    |> Expect.equal False
        ]
