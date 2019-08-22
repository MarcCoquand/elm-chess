module MovesTest exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Moves exposing (Moves, Valid)
import Player
import Predicate exposing (Predicate)
import Test exposing (..)


type alias Position =
    ( Int, Int )


alwaysTrue : Predicate Position
alwaysTrue =
    Predicate.make (\_ -> True)


alwaysFalse : Predicate Position
alwaysFalse =
    Predicate.make (\_ -> False)


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


positionGenerator : Fuzzer ( Int, Int )
positionGenerator =
    Fuzz.tuple
        ( Fuzz.intRange boardMin boardMax, Fuzz.intRange boardMin boardMax )


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
                    , position = ( x, y )
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
                    , position = ( x, y )
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
                    , position = ( x, y )
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
                    , isBlank =
                        Predicate.make (\_ -> True)
                    , collision = alwaysFalse
                    , outOfBounds = alwaysFalse
                    , hasMoved = False
                    , position = ( x, y )
                    }
                    |> Moves.member ( x, y + 2 )
                    |> Expect.equal True
        , fuzz positionGenerator "Pawn can not move two if moved before" <|
            \( x, y ) ->
                Moves.pawn
                    { player = Player.White
                    , belongsToPlayer = alwaysFalse
                    , isBlank = alwaysTrue
                    , collision = alwaysFalse
                    , outOfBounds = alwaysFalse
                    , hasMoved = True
                    , position = ( x, y )
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
                    , position = ( x, y )
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
                    , position = ( x, y )
                    }
                    |> Moves.checkAll inRange
                    |> Expect.equal True
        ]


bishop : Test
bishop =
    describe "Ensure bishop moves correctly"
        [ fuzz positionGenerator "bishop can never move forward" <|
            \( x, y ) ->
                Moves.bishop
                    { belongsToPlayer = alwaysFalse
                    , outOfBounds = alwaysFalse
                    , position = ( x, y )
                    , isCollision = alwaysFalse
                    }
                    |> any
                        [ ( x, y - 1 )
                        , ( x, y + 1 )
                        ]
                    |> Expect.equal False
        , fuzz positionGenerator "bishop can never move out of bounds" <|
            \( x, y ) ->
                Moves.bishop
                    { belongsToPlayer = alwaysFalse
                    , outOfBounds = Predicate.make (not << inRange)
                    , position = ( x, y )
                    , isCollision = alwaysFalse
                    }
                    |> Moves.checkAll inRange
                    |> Expect.equal True
        ]


suite : Test
suite =
    Test.concat [ pawn, bishop ]
