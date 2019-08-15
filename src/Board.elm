module Board exposing (Board(..), Coordinate, construct, display, filter, filterExtract, foldr, get, inBounds, isLegalMove, isWithinRange, makeIndex, numberList, set, size, toIndexedList, toString, update, upperBound)

import Array exposing (Array)
import Set exposing (Set)
import String


type alias Coordinate =
    ( Int, Int )


type Board square
    = CreateBoard (Array (Array square))


toString : (e -> String) -> Board e -> List String
toString show (CreateBoard board) =
    let
        showRow y =
            Array.map show y
                |> Array.toList
                |> String.join " "
    in
    Array.map showRow board
        |> Array.toList


isLegalMove : Set Coordinate -> Coordinate -> Bool
isLegalMove legalMoves destination =
    Set.member destination legalMoves


numberList : Int -> Int -> String
numberList amount startDigit =
    List.range startDigit (startDigit + amount)
        |> List.map String.fromInt
        |> String.join ". "


makeIndex : List String -> List String
makeIndex board =
    let
        numberedTopRow =
            "   " ++ numberList (List.length board) 1

        appendNumber n string =
            String.fromInt n ++ ". " ++ string

        appendRow n row =
            appendNumber n row

        numberedBoardRows =
            -- Prefer foldl' over foldl for efficiency and avoiding space leaks.
            List.indexedMap appendRow board
    in
    numberedTopRow :: numberedBoardRows



{- Folds board row by row -}


toIndexedList : Board square -> List ( Coordinate, square )
toIndexedList (CreateBoard board) =
    Array.toIndexedList board
        |> List.map (\( column, arr ) -> ( column, Array.toIndexedList arr ))
        |> List.map
            (\( column, rowAndElement ) ->
                List.map (\( row, element ) -> ( ( column, row ), element ))
                    rowAndElement
            )
        |> List.concat


foldr : (a -> b -> b) -> b -> Board a -> b
foldr folder initial (CreateBoard board) =
    let
        foldRow row i =
            Array.foldr folder i row
    in
    Array.foldr foldRow initial board


display : (e -> String) -> Board e -> String
display show board =
    toString show board
        |> makeIndex
        |> String.join "\n"


size : Int
size =
    8


upperBound : Board e -> ( Int, Int )
upperBound (CreateBoard board) =
    let
        upper =
            Array.length board
    in
    ( upper, upper )


inBounds : ( Int, Int ) -> Bool
inBounds coord =
    isWithinRange coord ( size, size )


isWithinRange : ( Int, Int ) -> ( Int, Int ) -> Bool
isWithinRange ( c1, c2 ) ( c3, c4 ) =
    c1 <= c3 && c2 <= c4 && c1 > 0 && c2 > 0


get : Board square -> Coordinate -> Maybe square
get (CreateBoard board) ( row, column ) =
    Array.get column board
        |> Maybe.andThen (Array.get row)


set : ( Coordinate, square ) -> Board square -> Board square
set ( ( x, y ), square ) (CreateBoard board) =
    Array.get y board
        |> Maybe.map (Array.set x square)
        |> Maybe.map (\row -> Array.set x row board)
        |> Maybe.map CreateBoard
        |> Maybe.withDefault (CreateBoard board)


update : Board square -> List ( Coordinate, square ) -> Board square
update board changes =
    List.foldr set board changes


filter : (square -> Bool) -> Board square -> List square
filter isCondition board =
    let
        conditionalAppend square list =
            if isCondition square then
                square :: list

            else
                list
    in
    foldr conditionalAppend [] board


filterExtract : (square -> Maybe a) -> Board square -> List a
filterExtract filterFunction board =
    let
        conditionalAppend square list =
            case filterFunction square of
                Just a ->
                    a :: list

                Nothing ->
                    list
    in
    foldr conditionalAppend [] board


construct : List (List square) -> Maybe (Board square)
construct board =
    let
        availableSlots =
            size * size
    in
    -- Crash the program if the board is badly constructed
    if
        List.length (List.concat board)
            == availableSlots
            && List.length board
            == size
    then
        List.map Array.fromList board
            |> Array.fromList
            |> (\arrayBoard -> Just (CreateBoard arrayBoard))

    else
        Nothing
