module Move exposing
    ( Move(..)
    , bishop
    , highlight
    , isValid
    , king
    , knight
    , pawn
    , queen
    , rook
    , toList
    )

import Highlight exposing (Highlight)
import Move.Ruleset as Ruleset exposing (Ruleset)
import Player exposing (Player(..))
import Position exposing (Position)
import Predicate exposing (Predicate)
import Set exposing (Set)
import Set.Extra exposing (unions)


type Move
    = Single { from : Position, to : Position }
    | Swap { from : Position, to : Position, swapFrom : Position, swapTo : Position }
    | Change { from : Position, to : Position }
    | Invalid


type alias HasMoved =
    Bool


highlight : Move -> Highlight
highlight move =
    case move of
        Single _ ->
            Highlight.Blue

        Swap _ ->
            Highlight.Red

        Change _ ->
            Highlight.Green

        Invalid ->
            Highlight.None


isValid : Move -> Bool
isValid move =
    move /= Invalid


makeSingle : Position -> Ruleset Ruleset.Valid -> Position -> Move
makeSingle from moves to =
    if Ruleset.member to moves then
        Single { from = from, to = to }

    else
        Invalid


toList : Move -> List { from : Position, to : Position }
toList move =
    case move of
        Single m ->
            [ m ]

        Swap { from, to, swapFrom, swapTo } ->
            [ { from = from, to = to }, { from = swapFrom, to = swapTo } ]

        Change m ->
            [ m ]

        Invalid ->
            []



-- BISHOP


bishop :
    { belongsToPlayer : Predicate Position
    , outOfBounds : Predicate Position
    , collision : Predicate Position
    , position : Position
    }
    -> (Position -> Move)
bishop arguments =
    Ruleset.bishop arguments
        |> makeSingle arguments.position



-- KING


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
king arguments =
    let
        validRuleset =
            Ruleset.king arguments
    in
    \to ->
        case maybeSwapRook { from = arguments.position, to = to } of
            Just rookMove ->
                if Ruleset.member to validRuleset then
                    Swap
                        { from = arguments.position
                        , to = to
                        , swapFrom = rookMove.swapFrom
                        , swapTo =
                            rookMove.swapTo
                        }

                else
                    Invalid

            Nothing ->
                makeSingle arguments.position validRuleset to


maybeSwapRook :
    { from : Position, to : Position }
    -> Maybe { swapFrom : Position, swapTo : Position }
maybeSwapRook { from, to } =
    let
        ( x, y ) =
            from
    in
    if to == ( x - 2, y ) then
        Just { swapFrom = ( x - 3, y ), swapTo = ( x - 1, y ) }

    else if to == ( x + 3, y ) then
        Just
            { swapFrom = Position.make { x = x + 4, y = y }
            , swapTo = Position.make { x = x + 2, y = y }
            }

    else
        Nothing



-- KNIGHT


knight :
    { belongsToPlayer : Predicate Position
    , outOfBounds : Predicate Position
    , position : Position
    }
    -> (Position -> Move)
knight arguments =
    Ruleset.knight arguments
        |> makeSingle arguments.position



-- PAWN


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
pawn arguments =
    Ruleset.pawn arguments
        |> makeSingle arguments.position



-- QUEEN


queen :
    { belongsToPlayer : Predicate Position
    , outOfBounds : Predicate Position
    , collision : Predicate Position
    , position : Position
    }
    -> (Position -> Move)
queen arguments =
    Ruleset.queen arguments
        |> makeSingle arguments.position



-- ROOK


rook :
    { belongsToPlayer : Predicate Position
    , outOfBounds : Predicate Position
    , position : Position
    , collision : Predicate Position
    }
    -> (Position -> Move)
rook arguments =
    Ruleset.rook arguments
        |> makeSingle arguments.position
