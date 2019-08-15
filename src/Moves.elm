module Moves exposing
    ( All
    , Coordinate
    , Illegal
    , Moves
    , Valid
    , bishop
    , king
    , knight
    , member
    , pawn
    , queen
    , remove
    , rook
    , rules
    )

import Player exposing (Player(..))
import Predicate
    exposing
        ( BelongsToPlayer
        , IsBlank
        , IsCollision
        , OutOfBounds
        , Predicate
        , Swappable
        , Threatened
        )
import Set exposing (Set)
import Set.Extra exposing (unions)


type Moves a
    = Moves (Set Coordinate)


type alias HasMoved =
    Bool


type alias Coordinate =
    ( Int, Int )


empty : Moves a
empty =
    Moves Set.empty


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


rules : List (Moves a) -> Moves a
rules =
    -- Same as unions
    List.foldl union empty



-- BISHOP


bishop :
    { belongsToPlayer : Predicate Coordinate BelongsToPlayer
    , outOfBounds : Predicate Coordinate OutOfBounds
    , coord : Coordinate
    }
    -> Moves Valid
bishop { belongsToPlayer, outOfBounds, coord } =
    let
        all =
            allBishop coord

        illegalMoves =
            rules
                [ outOfBoundsRule outOfBounds all
                , attackOwnPieceRule belongsToPlayer all
                ]
    in
    remove illegalMoves all


allBishop : Coordinate -> Moves All
allBishop ( x, y ) =
    let
        positiveDiagonal =
            List.range -8 8 |> List.map (\value -> ( value + x, value + y ))

        negativeDiagonal =
            List.range -8 8 |> List.map (\value -> ( value + x, y - value ))

        currentPosition =
            Set.singleton ( x, y )
    in
    positiveDiagonal
        ++ negativeDiagonal
        |> Set.fromList
        |> (\set -> Set.diff set currentPosition)
        |> Moves


member : Coordinate -> Moves Valid -> Bool
member coord (Moves move) =
    Set.member coord move



-- KING


kingThreatenedRule : Predicate Coordinate Threatened -> Moves All -> Moves Illegal
kingThreatenedRule isThreatened (Moves all) =
    Set.filter (Predicate.check isThreatened) all
        |> Moves


kingSwapRightRule : Predicate Coordinate Swappable -> Coordinate -> Moves Illegal
kingSwapRightRule isSwappable ( x, y ) =
    if Predicate.check isSwappable ( x, y ) then
        Set.empty
            |> Moves

    else
        Set.singleton ( x + 2, y )
            |> Moves


kingSwapLeftRule : Predicate Coordinate Swappable -> Coordinate -> Moves Illegal
kingSwapLeftRule isSwappable ( x, y ) =
    if Predicate.check isSwappable ( x, y ) then
        Set.empty
            |> Moves

    else
        Set.singleton ( x - 3, y )
            |> Moves


king :
    { belongsToPlayer : Predicate Coordinate BelongsToPlayer
    , outOfBounds : Predicate Coordinate OutOfBounds
    , isThreatened : Predicate Coordinate Threatened
    , coord : Coordinate
    , swapRight : Predicate Coordinate Swappable
    , swapLeft : Predicate Coordinate Swappable
    }
    -> Moves Valid
king { belongsToPlayer, outOfBounds, isThreatened, coord, swapRight, swapLeft } =
    let
        all =
            allKing coord

        illegalMoves =
            rules
                [ outOfBoundsRule outOfBounds all
                , attackOwnPieceRule belongsToPlayer all
                , kingThreatenedRule isThreatened all
                , kingSwapRightRule swapRight coord
                , kingSwapLeftRule swapLeft coord
                ]
    in
    remove illegalMoves all


allKing : Coordinate -> Moves All
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
    { belongsToPlayer : Predicate Coordinate BelongsToPlayer
    , outOfBounds : Predicate Coordinate OutOfBounds
    , coord : Coordinate
    }
    -> Moves Valid
knight { belongsToPlayer, outOfBounds, coord } =
    let
        all =
            allKnight coord

        illegalMoves =
            rules
                [ outOfBoundsRule outOfBounds all
                , attackOwnPieceRule belongsToPlayer all
                ]
    in
    remove illegalMoves all


allKnight : Coordinate -> Moves All
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


southRule : Coordinate -> Moves Illegal
southRule ( x, y ) =
    Set.fromList
        [ ( x - 1, y + 1 )
        , ( x - 1, y - 1 )
        , ( x - 2, y )
        , ( x - 1, y )
        ]
        |> Moves


northRule : Coordinate -> Moves Illegal
northRule ( x, y ) =
    Set.fromList [ ( x + 1, y + 1 ), ( x + 1, y - 1 ), ( x + 2, y ), ( x + 1, y ) ]
        |> Moves


pawnDoubleMoveRule : Bool -> Coordinate -> Moves Illegal
pawnDoubleMoveRule hasMoved ( x, y ) =
    let
        initial =
            Set.fromList [ ( x + 2, y ), ( x - 2, y ) ]
    in
    if hasMoved then
        initial
            |> Moves

    else
        Set.empty
            |> Moves


pawnDirectionRule : Player -> Coordinate -> Moves Illegal
pawnDirectionRule player coords =
    case getDirection player of
        North ->
            southRule coords

        South ->
            northRule coords


pawnAttackRules :
    Predicate Coordinate IsBlank
    -> Coordinate
    -> Moves Illegal
pawnAttackRules isEmpty ( x, y ) =
    let
        attackRange =
            Set.fromList
                [ ( x + 1, y + 1 )
                , ( x + 1, y - 1 )
                , ( x - 1, y + 1 )
                , ( x - 1, y - 1 )
                ]
    in
    Set.filter
        (\coordinate ->
            Predicate.check isEmpty coordinate
        )
        attackRange
        |> Moves


pawnCollisionRule : Predicate Coordinate IsCollision -> Coordinate -> Moves Illegal
pawnCollisionRule isCollision ( x, y ) =
    let
        set =
            Set.fromList [ ( x + 1, y ), ( x + 2, y ), ( x - 1, y ), ( x - 2, y ) ]
    in
    Set.filter (Predicate.check isCollision) set
        |> Moves


outOfBoundsRule : Predicate Coordinate OutOfBounds -> Moves All -> Moves Illegal
outOfBoundsRule isOutOfBounds (Moves allMoves) =
    Set.filter (Predicate.check isOutOfBounds) allMoves
        |> Moves


attackOwnPieceRule : Predicate Coordinate BelongsToPlayer -> Moves All -> Moves Illegal
attackOwnPieceRule belongsToPlayer (Moves allMoves) =
    Set.filter (Predicate.check belongsToPlayer) allMoves
        |> Moves


pawn :
    { player : Player
    , belongsToPlayer : Predicate Coordinate BelongsToPlayer
    , isBlank : Predicate Coordinate IsBlank
    , collision : Predicate Coordinate IsCollision
    , outOfBounds : Predicate Coordinate OutOfBounds
    , hasMoved : Bool
    , coord :
        Coordinate
    }
    -> Moves Valid
pawn { player, belongsToPlayer, isBlank, collision, outOfBounds, hasMoved, coord } =
    let
        all =
            allPawn coord

        illegalMoves =
            rules
                [ pawnCollisionRule collision coord
                , pawnAttackRules isBlank coord
                , pawnDirectionRule player coord
                , pawnDoubleMoveRule hasMoved coord
                , attackOwnPieceRule belongsToPlayer all
                ]
    in
    remove illegalMoves all


allPawn : Coordinate -> Moves All
allPawn ( x, y ) =
    Set.fromList
        [ ( x + 1, y + 1 )
        , ( x + 1, y - 1 )
        , ( x + 2, y )
        , ( x + 1, y )
        , ( x - 1, y + 1 )
        , ( x - 1, y - 1 )
        , ( x - 2, y )
        , ( x - 1, y )
        ]
        |> Moves



-- QUEEN


queen :
    { belongsToPlayer : Predicate Coordinate BelongsToPlayer
    , outOfBounds : Predicate Coordinate OutOfBounds
    , coord : Coordinate
    }
    -> Moves Valid
queen { belongsToPlayer, outOfBounds, coord } =
    let
        all =
            allQueen coord

        illegalMoves =
            rules
                [ outOfBoundsRule outOfBounds all
                , attackOwnPieceRule belongsToPlayer all
                ]
    in
    remove illegalMoves all


allQueen : Coordinate -> Moves All
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
    { belongsToPlayer : Predicate Coordinate BelongsToPlayer
    , outOfBounds : Predicate Coordinate OutOfBounds
    , coord : Coordinate
    }
    -> Moves Valid
rook { belongsToPlayer, outOfBounds, coord } =
    let
        all =
            allRook coord

        illegalMoves =
            rules
                [ outOfBoundsRule outOfBounds all
                , attackOwnPieceRule belongsToPlayer all
                ]
    in
    remove illegalMoves all


allRook : Coordinate -> Moves All
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
