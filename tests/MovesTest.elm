module MovesTest exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Moves exposing (Moves, Valid)
import Player
import Position exposing (Position)
import Predicate exposing (Predicate)
import Test exposing (..)


alwaysTrue : Predicate Position
alwaysTrue =
    \_ -> True


alwaysFalse : Predicate Position
alwaysFalse =
    \_ -> False


boardMin =
    0


boardMax =
    7


inRange : Position -> Bool
inRange ( x, y ) =
    x
        >= boardMin
        && y
        >= boardMin
        && x
        <= boardMax
        && y
        <= boardMax


positionGenerator : Fuzzer Position
positionGenerator =
    Fuzz.tuple
        ( Fuzz.intRange boardMin boardMax, Fuzz.intRange boardMin boardMax )
        |> Fuzz.map (\( x, y ) -> Position.make { x = x, y = y })


members : List Position -> Moves Valid -> Bool
members position moves =
    List.all (\pos -> Moves.member pos moves) position


any : List Position -> Moves Valid -> Bool
any position moves =
    List.any (\pos -> Moves.member pos moves) position


expectMember pos move =
    Expect.equal True (Moves.member pos move)


pawn : Test
pawn =
    describe "Ensure pawn moves correctly"
        [ fuzz positionGenerator "Pawn can not move up if White" <|
            \( x, y ) ->
                Moves.pawn
                    { player = Player.White
                    , belongsToPlayer = alwaysFalse
                    , isBlank = alwaysFalse
                    , collision = alwaysFalse
                    , outOfBounds = alwaysFalse
                    , hasMoved = False
                    , position = Position.make { x = x, y = y }
                    }
                    |> members
                        [ ( x, y - 1 )
                        , ( x, y - 2 )
                        , ( x + 1, y - 1 )
                        , ( x - 1, y - 1 )
                        ]
                    |> Expect.equal False
        , fuzz positionGenerator "Pawn can not move down if Black" <|
            \( x, y ) ->
                Moves.pawn
                    { player = Player.Black
                    , belongsToPlayer = alwaysFalse
                    , isBlank = alwaysFalse
                    , collision = alwaysFalse
                    , outOfBounds = alwaysFalse
                    , hasMoved = False
                    , position = Position.make { x = x, y = y }
                    }
                    |> members
                        [ ( x, y + 1 )
                        , ( x, y + 2 )
                        , ( x + 1, y + 1 )
                        , ( x - 1, y + 1 )
                        ]
                    |> Expect.equal False
        , fuzz positionGenerator "Pawn can not move forward if colission" <|
            \( x, y ) ->
                Moves.pawn
                    { player = Player.White
                    , belongsToPlayer = alwaysFalse
                    , isBlank =
                        Predicate.make (\_ -> True)
                    , collision = alwaysFalse
                    , outOfBounds = alwaysFalse
                    , hasMoved = False
                    , position = Position.make { x = x, y = y }
                    }
                    |> any
                        [ ( x, y - 1 )
                        , ( x, y - 2 )
                        ]
                    |> Expect.equal False
        , fuzz positionGenerator "Pawn can move two if not moved before" <|
            \( x, y ) ->
                Moves.pawn
                    { player = Player.White
                    , belongsToPlayer = alwaysFalse
                    , isBlank = alwaysFalse
                    , collision = alwaysFalse
                    , outOfBounds = alwaysFalse
                    , hasMoved = False
                    , position = Position.make { x = x, y = y }
                    }
                    |> Moves.member (Position.make { x = x, y = y + 2 })
                    |> Expect.equal True
        , fuzz positionGenerator "Pawn can not move two if moved before" <|
            \( x, y ) ->
                Moves.pawn
                    { player = Player.White
                    , belongsToPlayer = alwaysFalse
                    , isBlank = alwaysFalse
                    , collision = alwaysFalse
                    , outOfBounds = alwaysFalse
                    , hasMoved = True
                    , position = Position.make { x = x, y = y }
                    }
                    |> Moves.member ( x, y + 2 )
                    |> Expect.equal False
        , fuzz positionGenerator "Pawn can not attack own players" <|
            \( x, y ) ->
                Moves.pawn
                    { player = Player.White
                    , belongsToPlayer = alwaysTrue
                    , isBlank = alwaysFalse
                    , collision = alwaysFalse
                    , outOfBounds = alwaysFalse
                    , hasMoved = False
                    , position = Position.make { x = x, y = y }
                    }
                    |> any
                        [ ( x - 1, y - 1 )
                        , ( x + 1, y - 1 )
                        ]
                    |> Expect.equal False
        , fuzz positionGenerator "Pawn can not move out of bounds" <|
            \( x, y ) ->
                Moves.pawn
                    { player = Player.White
                    , belongsToPlayer = alwaysFalse
                    , isBlank = alwaysFalse
                    , collision = alwaysFalse
                    , outOfBounds = Predicate.make (not << inRange)
                    , hasMoved = False
                    , position = Position.make { x = x, y = y }
                    }
                    |> Moves.checkAll inRange
                    |> Expect.equal True
        ]


bishop : Test
bishop =
    describe "Ensure bishop moves correctly"
        [ fuzz positionGenerator "bishop can never move out of bounds" <|
            \( x, y ) ->
                Moves.bishop
                    { belongsToPlayer = alwaysFalse
                    , outOfBounds = Predicate.make (not << inRange)
                    , position = Position.make { x = x, y = y }
                    , collision = alwaysFalse
                    }
                    |> Moves.checkAll inRange
                    |> Expect.equal True
        ]


suite : Test
suite =
    Test.concat [ pawn, bishop ]
