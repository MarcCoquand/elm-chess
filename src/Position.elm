module Position exposing
    ( Position
    , getX
    , getY
    , isClosest
    , make
    , makeMany
    , member
    )

import Predicate exposing (Predicate)
import Set exposing (Set)


type alias Position =
    ( Int, Int )


make : { x : Int, y : Int } -> Position
make { x, y } =
    ( x, y )


getX : Position -> Int
getX ( x, _ ) =
    x


getY : Position -> Int
getY ( _, y ) =
    y


makeMany : List { x : Int, y : Int } -> Set Position
makeMany positions =
    positions
        |> List.map (\{ x, y } -> ( x, y ))
        |> Set.fromList


member : { x : Int, y : Int } -> Set Position -> Bool
member { x, y } positions =
    Set.member ( x, y ) positions


getStepCloser : { start : Position, end : Position } -> Position
getStepCloser { start, end } =
    let
        ( xStart, yStart ) =
            start

        ( xEnd, yEnd ) =
            end

        ( x, y ) =
            ( xStart - xEnd, yStart - yEnd )

        getCloser c =
            if c > 0 then
                c - 1

            else if c < 0 then
                c + 1

            else
                c
    in
    ( getCloser x, getCloser y )


{-|

    Check, given a collision predicate, that a given position is the closest

-}
isClosest :
    { isCollision : Predicate Position
    , start : Position
    , end :
        Position
    }
    -> Bool
isClosest { isCollision, start, end } =
    case getStepCloser { start = start, end = end } of
        ( 0, 0 ) ->
            True

        ( x, y ) ->
            Predicate.check isCollision ( x, y )
                && isClosest
                    { isCollision = isCollision
                    , start =
                        start
                    , end = ( x, y )
                    }



-- behind : Position -> Position -> Bool
-- behind current toCheck  =
-- let
-- (x,y) =
-- (current.x - toCheck.x, current.y - toCheck.y)
-- in
-- case (x,y) of
-- x < 0 && y < 0 ->
