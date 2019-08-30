module Move exposing
    ( Move(..)
    , bishop
    , checkAll
    , contains
    , empty
    , isValid
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


type Move
    = Single { from : Position, to : Position }
    | Swap { from : Position, to : Position, swapFrom : Position, swapTo : Position }
    | Invalid


type RuleSet a
    = RuleSet (Set Position)


type alias HasMoved =
    Bool


isValid : Move -> Bool
isValid move =
    move /= Invalid


makeSingle : Position -> RuleSet Valid -> Position -> Move
makeSingle from moves to =
    if member to moves then
        Single { from = from, to = to }

    else
        Invalid


empty : RuleSet a
empty =
    RuleSet Set.empty


checkAll : (Position -> Bool) -> RuleSet a -> Bool
checkAll condition (RuleSet moves) =
    Set.filter (not << condition) moves
        |> (\set -> set == Set.empty)


union : RuleSet a -> RuleSet a -> RuleSet a
union (RuleSet a) (RuleSet b) =
    Set.union a b |> RuleSet


remove : RuleSet Illegal -> RuleSet All -> RuleSet Valid
remove (RuleSet illegal) (RuleSet all) =
    Set.diff all illegal
        |> RuleSet


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


contains : Position -> List (RuleSet Valid) -> Bool
contains position moves =
    rules moves
        |> member position


rules : List (RuleSet a) -> RuleSet a
rules =
    -- Same as unions
    List.foldl union empty


rulesetFromList : List Position -> RuleSet a
rulesetFromList moves =
    Set.fromList moves |> RuleSet


toList : Move -> List { from : Position, to : Position }
toList move =
    case move of
        Single m ->
            [ m ]

        Swap { from, to, swapFrom, swapTo } ->
            [ { from = from, to = to }, { from = swapFrom, to = swapTo } ]

        Invalid ->
            []



-- If collision it should not be able to move behind the piece


collisionRule : Predicate Position -> RuleSet All -> Position -> RuleSet Illegal
collisionRule isCollision allRuleSet position =
    let
        (RuleSet all) =
            allRuleSet
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
        |> RuleSet



-- BISHOP


bishop :
    { belongsToPlayer : Predicate Position
    , outOfBounds : Predicate Position
    , collision : Predicate Position
    , position : Position
    }
    -> (Position -> Move)
bishop { belongsToPlayer, outOfBounds, collision, position } =
    let
        all =
            allBishop position

        illegalRuleSet =
            rules
                [ outOfBoundsRule outOfBounds all
                , attackOwnPieceRule belongsToPlayer all
                , collisionRule collision all position
                ]
    in
    remove illegalRuleSet all
        |> makeSingle position


allBishop : Position -> RuleSet All
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
        |> RuleSet


member : Position -> RuleSet Valid -> Bool
member position (RuleSet move) =
    Set.member position move



-- KING


kingThreatenedRule : Predicate Position -> RuleSet All -> RuleSet Illegal
kingThreatenedRule isThreatened (RuleSet all) =
    Set.filter (Predicate.check isThreatened) all
        |> RuleSet


kingSwapRightRule : Predicate Position -> Position -> RuleSet Illegal
kingSwapRightRule isSwappable ( x, y ) =
    if Predicate.check isSwappable ( x, y ) then
        Set.empty
            |> RuleSet

    else
        Set.singleton ( x + 3, y )
            |> RuleSet


kingSwapLeftRule : Predicate Position -> Position -> RuleSet Illegal
kingSwapLeftRule isSwappable ( x, y ) =
    if Predicate.check isSwappable ( x, y ) then
        Set.empty
            |> RuleSet

    else
        Set.singleton ( x - 2, y )
            |> RuleSet


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
    -> (Position -> Move)
king { belongsToPlayer, outOfBounds, isThreatened, position, swapRight, swapLeft } =
    let
        ( all, swapRook ) =
            allKing position

        illegalRuleSet =
            rules
                [ outOfBoundsRule outOfBounds all
                , attackOwnPieceRule belongsToPlayer all
                , kingThreatenedRule isThreatened all
                , kingSwapRightRule swapRight position
                , kingSwapLeftRule swapLeft position
                ]

        validRuleSet =
            remove illegalRuleSet all
    in
    \to ->
        case swapRook to of
            Just rookMove ->
                if member to validRuleSet then
                    Swap
                        { from = position
                        , to = to
                        , swapFrom = rookMove.from
                        , swapTo =
                            rookMove.to
                        }

                else
                    Invalid

            Nothing ->
                makeSingle position validRuleSet to


allKing : Position -> ( RuleSet All, Position -> Maybe { from : Position, to : Position } )
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
                |> RuleSet
    in
    ( validMoves, swapRook )



-- KNIGHT


knight :
    { belongsToPlayer : Predicate Position
    , outOfBounds : Predicate Position
    , position : Position
    }
    -> (Position -> Move)
knight { belongsToPlayer, outOfBounds, position } =
    let
        all =
            allKnight position

        illegalRuleSet =
            rules
                [ outOfBoundsRule outOfBounds all
                , attackOwnPieceRule belongsToPlayer all
                ]
    in
    remove illegalRuleSet all
        |> makeSingle position


allKnight : Position -> RuleSet All
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
        |> RuleSet



-- PAWN


onlyUpRule : Position -> RuleSet Illegal
onlyUpRule ( x, y ) =
    Set.fromList
        [ ( x + 1, y + 1 )
        , ( x - 1, y + 1 )
        , ( x, y + 2 )
        , ( x, y + 1 )
        ]
        |> RuleSet


onlyDownRule : Position -> RuleSet Illegal
onlyDownRule ( x, y ) =
    Set.fromList [ ( x + 1, y - 1 ), ( x - 1, y - 1 ), ( x, y - 2 ), ( x, y - 1 ) ]
        |> RuleSet


pawnDoubleMoveRule : Bool -> Position -> RuleSet Illegal
pawnDoubleMoveRule hasMoved ( x, y ) =
    let
        initial =
            Set.fromList [ ( x, y + 2 ), ( x, y - 2 ) ]
    in
    if hasMoved then
        initial
            |> RuleSet

    else
        Set.empty
            |> RuleSet


pawnDirectionRule : Player -> Position -> RuleSet Illegal
pawnDirectionRule player positions =
    case player of
        Black ->
            onlyUpRule positions

        White ->
            onlyDownRule positions


pawnAttackRules :
    Predicate Position
    -> Position
    -> RuleSet Illegal
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
    RuleSet (Set.filter (Predicate.check isBlank) attackRange)


pawnCollisionRule : Predicate Position -> Position -> RuleSet Illegal
pawnCollisionRule isCollision ( x, y ) =
    let
        set =
            Set.fromList [ ( x, y + 1 ), ( x, y + 2 ), ( x, y - 1 ), ( x, y - 2 ) ]
    in
    Set.filter (Predicate.check isCollision) set
        |> RuleSet


outOfBoundsRule : Predicate Position -> RuleSet All -> RuleSet Illegal
outOfBoundsRule isOutOfBounds (RuleSet allRuleSet) =
    Set.filter (Predicate.check isOutOfBounds) allRuleSet
        |> RuleSet


attackOwnPieceRule : Predicate Position -> RuleSet All -> RuleSet Illegal
attackOwnPieceRule belongsToPlayer (RuleSet allRuleSet) =
    Set.filter (Predicate.check belongsToPlayer) allRuleSet
        |> RuleSet


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
    -> (Position -> Move)
pawn { player, belongsToPlayer, isBlank, collision, outOfBounds, hasMoved, position } =
    let
        all =
            allPawn position

        illegalRuleSet =
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
    remove illegalRuleSet all
        |> makeSingle position


allPawn : Position -> RuleSet All
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
        |> RuleSet



-- QUEEN


queen :
    { belongsToPlayer : Predicate Position
    , outOfBounds : Predicate Position
    , collision : Predicate Position
    , position : Position
    }
    -> (Position -> Move)
queen { belongsToPlayer, outOfBounds, collision, position } =
    let
        all =
            allQueen position

        illegalRuleSet =
            rules
                [ outOfBoundsRule outOfBounds all
                , attackOwnPieceRule belongsToPlayer all
                , collisionRule collision all position
                ]
    in
    remove illegalRuleSet all
        |> makeSingle position


allQueen : Position -> RuleSet All
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
        |> RuleSet



-- ROOK


rook :
    { belongsToPlayer : Predicate Position
    , outOfBounds : Predicate Position
    , position : Position
    , collision : Predicate Position
    }
    -> (Position -> Move)
rook { belongsToPlayer, outOfBounds, position, collision } =
    let
        all =
            allRook position

        illegalRuleSet =
            rules
                [ outOfBoundsRule outOfBounds all
                , attackOwnPieceRule belongsToPlayer all
                , collisionRule collision all position
                ]
    in
    remove illegalRuleSet all
        |> makeSingle position


allRook : Position -> RuleSet All
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
        |> RuleSet
