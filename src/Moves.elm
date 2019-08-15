module Moves exposing
    ( All
    , Illegal
    , Moves
    , Position
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
    = Moves (Set Position)


type alias HasMoved =
    Bool


type alias Position =
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
    { belongsToPlayer : Predicate Position BelongsToPlayer
    , outOfBounds : Predicate Position OutOfBounds
    , position : Position
    }
    -> Moves Valid
bishop { belongsToPlayer, outOfBounds, position } =
    let
        all =
            allBishop position

        illegalMoves =
            rules
                [ outOfBoundsRule outOfBounds all
                , attackOwnPieceRule belongsToPlayer all
                ]
    in
    remove illegalMoves all


allBishop : Position -> Moves All
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


member : Position -> Moves Valid -> Bool
member position (Moves move) =
    Set.member position move



-- KING


kingThreatenedRule : Predicate Position Threatened -> Moves All -> Moves Illegal
kingThreatenedRule isThreatened (Moves all) =
    Set.filter (Predicate.check isThreatened) all
        |> Moves


kingSwapRightRule : Predicate Position Swappable -> Position -> Moves Illegal
kingSwapRightRule isSwappable ( x, y ) =
    if Predicate.check isSwappable ( x, y ) then
        Set.empty
            |> Moves

    else
        Set.singleton ( x + 2, y )
            |> Moves


kingSwapLeftRule : Predicate Position Swappable -> Position -> Moves Illegal
kingSwapLeftRule isSwappable ( x, y ) =
    if Predicate.check isSwappable ( x, y ) then
        Set.empty
            |> Moves

    else
        Set.singleton ( x - 3, y )
            |> Moves


king :
    { belongsToPlayer : Predicate Position BelongsToPlayer
    , outOfBounds : Predicate Position OutOfBounds
    , isThreatened : Predicate Position Threatened
    , position : Position
    , swapRight : Predicate Position Swappable
    , swapLeft : Predicate Position Swappable
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
    { belongsToPlayer : Predicate Position BelongsToPlayer
    , outOfBounds : Predicate Position OutOfBounds
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


southRule : Position -> Moves Illegal
southRule ( x, y ) =
    Set.fromList
        [ ( x - 1, y + 1 )
        , ( x - 1, y - 1 )
        , ( x - 2, y )
        , ( x - 1, y )
        ]
        |> Moves


northRule : Position -> Moves Illegal
northRule ( x, y ) =
    Set.fromList [ ( x + 1, y + 1 ), ( x + 1, y - 1 ), ( x + 2, y ), ( x + 1, y ) ]
        |> Moves


pawnDoubleMoveRule : Bool -> Position -> Moves Illegal
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


pawnDirectionRule : Player -> Position -> Moves Illegal
pawnDirectionRule player positions =
    case getDirection player of
        North ->
            southRule positions

        South ->
            northRule positions


pawnAttackRules :
    Predicate Position IsBlank
    -> Position
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
        (\positioninate ->
            Predicate.check isEmpty positioninate
        )
        attackRange
        |> Moves


pawnCollisionRule : Predicate Position IsCollision -> Position -> Moves Illegal
pawnCollisionRule isCollision ( x, y ) =
    let
        set =
            Set.fromList [ ( x + 1, y ), ( x + 2, y ), ( x - 1, y ), ( x - 2, y ) ]
    in
    Set.filter (Predicate.check isCollision) set
        |> Moves


outOfBoundsRule : Predicate Position OutOfBounds -> Moves All -> Moves Illegal
outOfBoundsRule isOutOfBounds (Moves allMoves) =
    Set.filter (Predicate.check isOutOfBounds) allMoves
        |> Moves


attackOwnPieceRule : Predicate Position BelongsToPlayer -> Moves All -> Moves Illegal
attackOwnPieceRule belongsToPlayer (Moves allMoves) =
    Set.filter (Predicate.check belongsToPlayer) allMoves
        |> Moves


pawn :
    { player : Player
    , belongsToPlayer : Predicate Position BelongsToPlayer
    , isBlank : Predicate Position IsBlank
    , collision : Predicate Position IsCollision
    , outOfBounds : Predicate Position OutOfBounds
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
                , attackOwnPieceRule belongsToPlayer all
                ]
    in
    remove illegalMoves all


allPawn : Position -> Moves All
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
    { belongsToPlayer : Predicate Position BelongsToPlayer
    , outOfBounds : Predicate Position OutOfBounds
    , position : Position
    }
    -> Moves Valid
queen { belongsToPlayer, outOfBounds, position } =
    let
        all =
            allQueen position

        illegalMoves =
            rules
                [ outOfBoundsRule outOfBounds all
                , attackOwnPieceRule belongsToPlayer all
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
    { belongsToPlayer : Predicate Position BelongsToPlayer
    , outOfBounds : Predicate Position OutOfBounds
    , position : Position
    }
    -> Moves Valid
rook { belongsToPlayer, outOfBounds, position } =
    let
        all =
            allRook position

        illegalMoves =
            rules
                [ outOfBoundsRule outOfBounds all
                , attackOwnPieceRule belongsToPlayer all
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
