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
member coordinate positions =
    Set.member (make coordinate) positions


stepCloser : { start : Position, end : Position } -> Position
stepCloser { start, end } =
    let
        ( xStart, yStart ) =
            start

        ( x, y ) =
            end

        ( differenceX, differenceY ) =
            ( xStart - x, yStart - y )

        getCloser difference n =
            if difference < 0 then
                n - 1

            else if difference > 0 then
                n + 1

            else
                n
    in
    ( getCloser differenceX x, getCloser differenceY y )


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
    let
        ( x, y ) =
            stepCloser { start = start, end = end }
    in
    if ( x, y ) == start then
        True

    else
        not (isCollision ( x, y ))
            && isClosest
                { isCollision = isCollision
                , start =
                    start
                , end = ( x, y )
                }
