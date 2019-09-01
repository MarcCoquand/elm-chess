module Move.Ruleset exposing
    ( All(..)
    , HasMoved
    , Illegal
    , Ruleset
    , Valid
    , bishop
    , checkMate
    , contains
    , empty
    , king
    , knight
    , member
    , pawn
    , queen
    , rook
    , rules
    , rulesetFromList
    , union
    )

import Player exposing (Player(..))
import Position exposing (Position)
import Predicate exposing (Predicate)
import Set exposing (Set)
import Set.Extra exposing (unions)


type Ruleset a
    = Ruleset (Set Position)


type alias HasMoved =
    Bool


isEmpty : Ruleset a -> Bool
isEmpty (Ruleset r) =
    Set.isEmpty r


empty : Ruleset a
empty =
    Ruleset Set.empty


member : Position -> Ruleset Valid -> Bool
member position (Ruleset move) =
    Set.member position move


union : Ruleset a -> Ruleset a -> Ruleset a
union (Ruleset a) (Ruleset b) =
    Set.union a b |> Ruleset


remove : Ruleset Illegal -> Ruleset All -> Ruleset a
remove (Ruleset illegal) (Ruleset all) =
    Set.diff all illegal
        |> Ruleset


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


contains : Position -> List (Ruleset Valid) -> Bool
contains position moves =
    rules moves
        |> member position


foldl : (Position -> b -> b) -> b -> Ruleset a -> b
foldl f b (Ruleset a) =
    Set.foldl f b a


rules : List (Ruleset a) -> Ruleset a
rules =
    -- Same as unions
    List.foldl union empty


rulesetFromList : List Position -> Ruleset a
rulesetFromList moves =
    Set.fromList moves |> Ruleset



-- If collision it should not be able to move behind the piece


collisionRule : Predicate Position -> Ruleset All -> Position -> Ruleset Illegal
collisionRule isCollision allRuleset position =
    let
        (Ruleset all) =
            allRuleset
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
        |> Ruleset



-- BISHOP


bishop :
    { belongsToPlayer : Predicate Position
    , outOfBounds : Predicate Position
    , collision : Predicate Position
    , position : Position
    }
    -> Ruleset Valid
bishop { belongsToPlayer, outOfBounds, collision, position } =
    let
        all =
            allBishop position

        illegalRuleset =
            rules
                [ outOfBoundsRule outOfBounds all
                , attackOwnPieceRule belongsToPlayer all
                , collisionRule collision all position
                ]
    in
    remove illegalRuleset all


allBishop : Position -> Ruleset All
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
        |> Ruleset



-- KING


kingThreatenedRule : Predicate Position -> Ruleset All -> Ruleset Illegal
kingThreatenedRule isThreatened (Ruleset all) =
    Set.filter (Predicate.check isThreatened) all
        |> Ruleset


kingSwapRightRule : Predicate Position -> Position -> Ruleset Illegal
kingSwapRightRule isSwappable ( x, y ) =
    if Predicate.check isSwappable ( x, y ) then
        Set.empty
            |> Ruleset

    else
        Set.singleton ( x + 3, y )
            |> Ruleset


kingSwapLeftRule : Predicate Position -> Position -> Ruleset Illegal
kingSwapLeftRule isSwappable ( x, y ) =
    if Predicate.check isSwappable ( x, y ) then
        Set.empty
            |> Ruleset

    else
        Set.singleton ( x - 2, y )
            |> Ruleset


{-|

    In the future this function should be generated for all of them to perform
    less checks.

-}
king :
    { belongsToPlayer : Predicate Position
    , outOfBounds : Predicate Position
    , isThreatened : Predicate Position
    , position : Position
    , swapRight : Predicate Position
    , swapLeft : Predicate Position
    }
    -> Ruleset Valid
king { belongsToPlayer, outOfBounds, isThreatened, position, swapRight, swapLeft } =
    let
        ( all, swapRook ) =
            allKing position

        illegalExceptThreatened =
            rules
                [ outOfBoundsRule outOfBounds all
                , attackOwnPieceRule belongsToPlayer all
                , kingSwapRightRule swapRight position
                , kingSwapLeftRule swapLeft position
                ]

        validExceptThreatened =
            remove illegalExceptThreatened all
    in
    -- This ugly workaround is due to the fact that isThreatened is usually very
    -- heavy to calculate so we just want to run it on the remaining pieces.
    remove (kingThreatenedRule isThreatened validExceptThreatened) validExceptThreatened


checkMate :
    { belongsToPlayer : Predicate Position
    , outOfBounds : Predicate Position
    , isThreatened : Predicate Position
    , position : Position
    , swapRight : Predicate Position
    , swapLeft : Predicate Position
    }
    -> Bool
checkMate { belongsToPlayer, outOfBounds, isThreatened, position, swapRight, swapLeft } =
    let
        ( all, swapRook ) =
            allKing position

        illegalRuleset =
            rules
                [ outOfBoundsRule outOfBounds all
                , attackOwnPieceRule belongsToPlayer all
                , kingThreatenedRule isThreatened all
                , kingSwapRightRule swapRight position
                , kingSwapLeftRule swapLeft position
                ]

        validMoves =
            remove illegalRuleset all
    in
    isEmpty validMoves && isThreatened position


allKing : Position -> ( Ruleset All, Position -> Maybe { from : Position, to : Position } )
allKing ( x, y ) =
    let
        column =
            List.range -1 1 |> List.map (\value -> ( x, value + y ))

        row =
            List.range -2 1 |> List.map (\value -> ( value + x, y ))

        positiveDiagonal =
            List.range -1 1 |> List.map (\value -> ( value + x, value + y ))

        negativeDiagonal =
            List.range -1 1 |> List.map (\value -> ( value + x, y - value ))

        currentPosition =
            Set.singleton ( x, y )

        swapRook position =
            if position == ( x - 2, y ) then
                Just { from = ( x - 3, y ), to = ( x - 1, y ) }

            else if position == ( x + 3, y ) then
                Just
                    { from = Position.make { x = x + 4, y = y }
                    , to = Position.make { x = x + 2, y = y }
                    }

            else
                Nothing

        validMoves =
            column
                ++ row
                ++ positiveDiagonal
                ++ negativeDiagonal
                |> Set.fromList
                -- include swap right
                |> Set.insert ( x + 3, y )
                |> (\set -> Set.diff set currentPosition)
                |> Ruleset
    in
    ( validMoves, swapRook )



-- KNIGHT


knight :
    { belongsToPlayer : Predicate Position
    , outOfBounds : Predicate Position
    , position : Position
    }
    -> Ruleset Valid
knight { belongsToPlayer, outOfBounds, position } =
    let
        all =
            allKnight position

        illegalRuleset =
            rules
                [ outOfBoundsRule outOfBounds all
                , attackOwnPieceRule belongsToPlayer all
                ]
    in
    remove illegalRuleset all


allKnight : Position -> Ruleset All
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
        |> Ruleset



-- PAWN


onlyUpRule : Position -> Ruleset Illegal
onlyUpRule ( x, y ) =
    Set.fromList
        [ ( x + 1, y + 1 )
        , ( x - 1, y + 1 )
        , ( x, y + 2 )
        , ( x, y + 1 )
        ]
        |> Ruleset


onlyDownRule : Position -> Ruleset Illegal
onlyDownRule ( x, y ) =
    Set.fromList [ ( x + 1, y - 1 ), ( x - 1, y - 1 ), ( x, y - 2 ), ( x, y - 1 ) ]
        |> Ruleset


pawnDoubleMoveRule : Bool -> Position -> Ruleset Illegal
pawnDoubleMoveRule hasMoved ( x, y ) =
    let
        initial =
            Set.fromList [ ( x, y + 2 ), ( x, y - 2 ) ]
    in
    if hasMoved then
        initial
            |> Ruleset

    else
        Set.empty
            |> Ruleset


pawnDirectionRule : Player -> Position -> Ruleset Illegal
pawnDirectionRule player positions =
    case player of
        Black ->
            onlyUpRule positions

        White ->
            onlyDownRule positions


pawnAttackRules :
    Predicate Position
    -> Position
    -> Ruleset Illegal
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
    Ruleset (Set.filter (Predicate.check isBlank) attackRange)


pawnCollisionRule : Predicate Position -> Position -> Ruleset Illegal
pawnCollisionRule isCollision ( x, y ) =
    let
        set =
            Set.fromList [ ( x, y + 1 ), ( x, y + 2 ), ( x, y - 1 ), ( x, y - 2 ) ]
    in
    Set.filter (Predicate.check isCollision) set
        |> Ruleset


outOfBoundsRule : Predicate Position -> Ruleset All -> Ruleset Illegal
outOfBoundsRule isOutOfBounds (Ruleset allRuleset) =
    Set.filter (Predicate.check isOutOfBounds) allRuleset
        |> Ruleset


attackOwnPieceRule : Predicate Position -> Ruleset All -> Ruleset Illegal
attackOwnPieceRule belongsToPlayer (Ruleset allRuleset) =
    Set.filter (Predicate.check belongsToPlayer) allRuleset
        |> Ruleset


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
    -> Ruleset Valid
pawn { player, belongsToPlayer, isBlank, collision, outOfBounds, hasMoved, position } =
    let
        all =
            allPawn position

        illegalRuleset =
            rules
                [ pawnCollisionRule collision position
                , pawnAttackRules isBlank position
                , pawnDirectionRule player position
                , pawnDoubleMoveRule hasMoved position
                , outOfBoundsRule outOfBounds all
                , attackOwnPieceRule belongsToPlayer all
                , collisionRule collision all position
                ]
    in
    remove illegalRuleset all


allPawn : Position -> Ruleset All
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
        |> Ruleset



-- QUEEN


queen :
    { belongsToPlayer : Predicate Position
    , outOfBounds : Predicate Position
    , collision : Predicate Position
    , position : Position
    }
    -> Ruleset Valid
queen { belongsToPlayer, outOfBounds, collision, position } =
    let
        all =
            allQueen position

        illegalRuleset =
            rules
                [ outOfBoundsRule outOfBounds all
                , attackOwnPieceRule belongsToPlayer all
                , collisionRule collision all position
                ]
    in
    remove illegalRuleset all


allQueen : Position -> Ruleset All
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
        |> Ruleset



-- ROOK


rook :
    { belongsToPlayer : Predicate Position
    , outOfBounds : Predicate Position
    , position : Position
    , collision : Predicate Position
    }
    -> Ruleset Valid
rook { belongsToPlayer, outOfBounds, position, collision } =
    let
        all =
            allRook position

        illegalRuleset =
            rules
                [ outOfBoundsRule outOfBounds all
                , attackOwnPieceRule belongsToPlayer all
                , collisionRule collision all position
                ]
    in
    remove illegalRuleset all


allRook : Position -> Ruleset All
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
        |> Ruleset
