module Moves exposing
    ( All
    , Illegal
    , Moves
    , Valid
    , bishop
    , checkAll
    , contains
    , empty
    , fromList
    , king
    , knight
    , member
    , pawn
    , queen
    , remove
    , rook
    , rules
    , toList
    )

import Player exposing (Player(..))
import Position exposing (Position)
import Predicate exposing (Predicate)
import Set exposing (Set)
import Set.Extra exposing (unions)


type Moves a
    = Moves (Set Position)


type alias HasMoved =
    Bool


empty : Moves a
empty =
    Moves Set.empty


checkAll : (Position -> Bool) -> Moves a -> Bool
checkAll condition (Moves moves) =
    Set.filter (not << condition) moves
        |> (\set -> set == Set.empty)


union : Moves a -> Moves a -> Moves a
union (Moves a) (Moves b) =
    Set.union a b |> Moves


remove : Moves Illegal -> Moves All -> Moves Valid
remove (Moves illegal) (Moves all) =
    Set.diff all illegal
        |> Moves


type Illegal
    = Illegal


type Valid
    = Valid


type All
    = All


type Direction
    = North
    | South


getDirection : Player -> Direction
getDirection player =
    case player of
        White ->
            South

        Black ->
            North


contains : Position -> List (Moves Valid) -> Bool
contains position moves =
    rules moves
        |> member position


rules : List (Moves a) -> Moves a
rules =
    -- Same as unions
    List.foldl union empty


fromList : List Position -> Moves a
fromList moves =
    Set.fromList moves |> Moves


toList : Moves a -> List Position
toList (Moves moves) =
    Set.toList moves



-- If collision it should not be able to move behind the piece


collisionRule :
    { isCollision : Predicate Position
    , allMoves : Moves All
    , position :
        Position
    }
    -> Moves Illegal
collisionRule { isCollision, allMoves, position } =
    let
        (Moves all) =
            allMoves
    in
    all
        |> Set.filter
            (\element ->
                not
                    (Position.isClosest
                        { isCollision = isCollision
                        , start = position
                        , end = element
                        }
                    )
            )
        |> Moves



-- BISHOP


bishop :
    { belongsToPlayer : Predicate Position
    , outOfBounds : Predicate Position
    , collision : Predicate Position
    , position : Position
    }
    -> Moves Valid
bishop { belongsToPlayer, outOfBounds, collision, position } =
    let
        all =
            allBishop position

        illegalMoves =
            rules
                [ outOfBoundsRule outOfBounds all
                , attackOwnPieceRule belongsToPlayer all
                , collisionRule
                    { isCollision = collision
                    , position = position
                    , allMoves = all
                    }
                ]
    in
    remove illegalMoves all


allBishop : Position -> Moves All
allBishop position =
    let
        positiveDiagonal =
            List.range -8 8
                |> List.map
                    (\value ->
                        Position.make
                            { x =
                                value
                                    + Position.getX position
                            , y = value + Position.getY position
                            }
                    )

        negativeDiagonal =
            List.range -8 8
                |> List.map
                    (\value ->
                        Position.make
                            { x = value + Position.getX position
                            , y = Position.getY position - value
                            }
                    )

        currentPosition =
            Set.singleton
                (Position.make
                    { x = Position.getX position
                    , y = Position.getY position
                    }
                )
    in
    positiveDiagonal
        ++ negativeDiagonal
        |> Set.fromList
        |> (\set -> Set.diff set currentPosition)
        |> Moves


member : Position -> Moves Valid -> Bool
member position (Moves move) =
    Set.member position move



-- KING


kingThreatenedRule : Predicate Position -> Moves All -> Moves Illegal
kingThreatenedRule isThreatened (Moves all) =
    Set.filter (Predicate.check isThreatened) all
        |> Moves


kingSwapRightRule : Predicate Position -> Position -> Moves Illegal
kingSwapRightRule isSwappable ( x, y ) =
    if Predicate.check isSwappable ( x, y ) then
        Set.empty
            |> Moves

    else
        Set.singleton ( x + 2, y )
            |> Moves


kingSwapLeftRule : Predicate Position -> Position -> Moves Illegal
kingSwapLeftRule isSwappable ( x, y ) =
    if Predicate.check isSwappable ( x, y ) then
        Set.empty
            |> Moves

    else
        Set.singleton ( x - 3, y )
            |> Moves


{-|

    Used to check if a king is threatened at a certain square because otherwise
    it will recursively run forever.

-}
kingWithoutThreatRule :
    { belongsToPlayer : Predicate Position
    , outOfBounds : Predicate Position
    , position : Position
    , swapRight : Predicate Position
    , swapLeft : Predicate Position
    }
    -> Moves Valid
kingWithoutThreatRule { belongsToPlayer, outOfBounds, position, swapRight, swapLeft } =
    let
        all =
            allKing position

        illegalMoves =
            rules
                [ outOfBoundsRule outOfBounds all
                , attackOwnPieceRule belongsToPlayer all
                , kingSwapRightRule swapRight position
                , kingSwapLeftRule swapLeft position
                ]
    in
    remove illegalMoves all


king :
    { belongsToPlayer : Predicate Position
    , outOfBounds : Predicate Position
    , isThreatened : Predicate Position
    , position : Position
    , swapRight : Predicate Position
    , swapLeft : Predicate Position
    }
    -> Moves Valid
king { belongsToPlayer, outOfBounds, isThreatened, position, swapRight, swapLeft } =
    let
        all =
            allKing position

        illegalMoves =
            rules
                [ outOfBoundsRule outOfBounds all
                , attackOwnPieceRule belongsToPlayer all
                , kingThreatenedRule isThreatened all
                , kingSwapRightRule swapRight position
                , kingSwapLeftRule swapLeft position
                ]
    in
    remove illegalMoves all


allKing : Position -> Moves All
allKing ( x, y ) =
    let
        column =
            List.range -1 1 |> List.map (\value -> ( x, value + y ))

        row =
            List.range -1 1 |> List.map (\value -> ( value + x, y ))

        positiveDiagonal =
            List.range -1 1 |> List.map (\value -> ( value + x, value + y ))

        negativeDiagonal =
            List.range -1 1 |> List.map (\value -> ( value + x, y - value ))

        currentPosition =
            Set.singleton ( x, y )
    in
    column
        ++ row
        ++ positiveDiagonal
        ++ negativeDiagonal
        |> Set.fromList
        -- For swapping rook / king
        |> Set.insert ( x + 2, y )
        |> Set.insert ( x - 3, y )
        |> (\set -> Set.diff set currentPosition)
        |> Moves



-- KNIGHT


knight :
    { belongsToPlayer : Predicate Position
    , outOfBounds : Predicate Position
    , position : Position
    }
    -> Moves Valid
knight { belongsToPlayer, outOfBounds, position } =
    let
        all =
            allKnight position

        illegalMoves =
            rules
                [ outOfBoundsRule outOfBounds all
                , attackOwnPieceRule belongsToPlayer all
                ]
    in
    remove illegalMoves all


allKnight : Position -> Moves All
allKnight ( x, y ) =
    Set.fromList
        [ ( x + 2, y + 1 )
        , ( x + 2, y - 1 )
        , ( x - 2, y + 1 )
        , ( x - 2, y - 1 )
        , ( x + 1, y + 2 )
        , ( x - 1, y + 2 )
        , ( x + 1, y - 2 )
        , ( x - 1, y - 2 )
        ]
        |> Moves



-- PAWN


onlyUpRule : Position -> Moves Illegal
onlyUpRule ( x, y ) =
    Set.fromList
        [ ( x + 1, y + 1 )
        , ( x - 1, y + 1 )
        , ( x, y + 2 )
        , ( x, y + 1 )
        ]
        |> Moves


onlyDownRule : Position -> Moves Illegal
onlyDownRule ( x, y ) =
    Set.fromList [ ( x + 1, y - 1 ), ( x - 1, y - 1 ), ( x, y - 2 ), ( x, y - 1 ) ]
        |> Moves


pawnDoubleMoveRule : Bool -> Position -> Moves Illegal
pawnDoubleMoveRule hasMoved ( x, y ) =
    let
        initial =
            Set.fromList [ ( x, y + 2 ), ( x, y - 2 ) ]
    in
    if hasMoved then
        initial
            |> Moves

    else
        Set.empty
            |> Moves


pawnDirectionRule : Player -> Position -> Moves Illegal
pawnDirectionRule player positions =
    case player of
        Black ->
            onlyUpRule positions

        White ->
            onlyDownRule positions


pawnAttackRules :
    Predicate Position
    -> Position
    -> Moves Illegal
pawnAttackRules isBlank ( x, y ) =
    let
        attackRange =
            Set.fromList
                [ ( x + 1, y + 1 )
                , ( x - 1, y + 1 )
                , ( x + 1, y - 1 )
                , ( x - 1, y - 1 )
                ]
    in
    Moves (Set.filter (Predicate.check isBlank) attackRange)


pawnCollisionRule : Predicate Position -> Position -> Moves Illegal
pawnCollisionRule isCollision ( x, y ) =
    let
        set =
            Set.fromList [ ( x, y + 1 ), ( x, y + 2 ), ( x, y - 1 ), ( x, y - 2 ) ]
    in
    Set.filter (Predicate.check isCollision) set
        |> Moves


outOfBoundsRule : Predicate Position -> Moves All -> Moves Illegal
outOfBoundsRule isOutOfBounds (Moves allMoves) =
    Set.filter (Predicate.check isOutOfBounds) allMoves
        |> Moves


attackOwnPieceRule : Predicate Position -> Moves All -> Moves Illegal
attackOwnPieceRule belongsToPlayer (Moves allMoves) =
    Set.filter (Predicate.check belongsToPlayer) allMoves
        |> Moves


pawn :
    { player : Player
    , belongsToPlayer : Predicate Position
    , isBlank : Predicate Position
    , collision : Predicate Position
    , outOfBounds : Predicate Position
    , hasMoved : Bool
    , position :
        Position
    }
    -> Moves Valid
pawn { player, belongsToPlayer, isBlank, collision, outOfBounds, hasMoved, position } =
    let
        all =
            allPawn position

        illegalMoves =
            rules
                [ pawnCollisionRule collision position
                , pawnAttackRules isBlank position
                , pawnDirectionRule player position
                , pawnDoubleMoveRule hasMoved position
                , outOfBoundsRule outOfBounds all
                , attackOwnPieceRule belongsToPlayer all
                , collisionRule
                    { isCollision = collision
                    , position = position
                    , allMoves = all
                    }
                ]
    in
    remove illegalMoves all


allPawn : Position -> Moves All
allPawn ( x, y ) =
    Set.fromList
        [ ( x, y + 1 )
        , ( x, y + 2 )
        , ( x, y - 1 )
        , ( x, y - 2 )
        , ( x + 1, y + 1 )
        , ( x - 1, y + 1 )
        , ( x + 1, y - 1 )
        , ( x - 1, y - 1 )
        ]
        |> Moves



-- QUEEN


queen :
    { belongsToPlayer : Predicate Position
    , outOfBounds : Predicate Position
    , collision : Predicate Position
    , position : Position
    }
    -> Moves Valid
queen { belongsToPlayer, outOfBounds, collision, position } =
    let
        all =
            allQueen position

        illegalMoves =
            rules
                [ outOfBoundsRule outOfBounds all
                , attackOwnPieceRule belongsToPlayer all
                , collisionRule
                    { isCollision = collision
                    , position = position
                    , allMoves = all
                    }
                ]
    in
    remove illegalMoves all


allQueen : Position -> Moves All
allQueen ( x, y ) =
    let
        column =
            List.range -8 8 |> List.map (\value -> ( x, value + y ))

        row =
            List.range -8 8 |> List.map (\value -> ( value + x, y ))

        positiveDiagonal =
            List.range -8 8 |> List.map (\value -> ( value + x, value + y ))

        negativeDiagonal =
            List.range -8 8 |> List.map (\value -> ( value + x, y - value ))

        currentPosition =
            Set.singleton ( x, y )
    in
    column
        ++ row
        ++ positiveDiagonal
        ++ negativeDiagonal
        |> Set.fromList
        |> (\set -> Set.diff set currentPosition)
        |> Moves



-- ROOK


rook :
    { belongsToPlayer : Predicate Position
    , outOfBounds : Predicate Position
    , position : Position
    , collision : Predicate Position
    }
    -> Moves Valid
rook { belongsToPlayer, outOfBounds, position, collision } =
    let
        all =
            allRook position

        illegalMoves =
            rules
                [ outOfBoundsRule outOfBounds all
                , attackOwnPieceRule belongsToPlayer all
                , collisionRule
                    { isCollision = collision
                    , position = position
                    , allMoves = all
                    }
                ]
    in
    remove illegalMoves all


allRook : Position -> Moves All
allRook ( x, y ) =
    let
        column =
            List.range -8 8 |> List.map (\value -> ( x, value + y ))

        row =
            List.range -8 8 |> List.map (\value -> ( value + x, y ))

        currentPosition =
            Set.singleton ( x, y )
    in
    column
        ++ row
        |> Set.fromList
        |> (\set -> Set.diff set currentPosition)
        |> Moves
