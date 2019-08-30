module Board exposing
    ( Board
    , construct
    , filter
    , filterExtract
    , foldr
    , get
    , inBounds
    , isWithinRange
    , makeIndex
    , numberList
    , positions
    , set
    , size
    , toIndexedList
    , update
    , upperBound
    , view
    )

import Array exposing (Array)
import Element exposing (Element)
import Element.Font as Font
import Position exposing (Position)
import Set exposing (Set)
import String


type Board square
    = CreateBoard (Array (Array square))


viewRow :
    { renderSquare : Int -> e -> msg -> Element msg
    , indexedMsg : Int -> msg
    , row : Array e
    }
    -> Element msg
viewRow { renderSquare, indexedMsg, row } =
    row
        |> Array.toIndexedList
        |> List.map
            (\( rowIndex, e ) ->
                renderSquare rowIndex e (indexedMsg rowIndex)
            )
        |> Element.row
            []


view :
    { renderSquare : Position -> e -> msg -> Element msg
    , indexedMsg : Position -> msg
    , board : Board e
    }
    -> Element msg
view { renderSquare, indexedMsg, board } =
    let
        (CreateBoard array) =
            board
    in
    array
        |> Array.toIndexedList
        |> List.map
            (\( columnIndex, row ) ->
                viewRow
                    { renderSquare =
                        \rowIndex ->
                            renderSquare
                                (Position.make { x = rowIndex, y = columnIndex })
                    , indexedMsg =
                        \rowIndex ->
                            indexedMsg
                                (Position.make
                                    { x = rowIndex, y = columnIndex }
                                )
                    , row = row
                    }
            )
        |> Element.column
            [ Font.size 50
            , Font.family [ Font.monospace ]
            ]


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
            List.indexedMap appendRow board
    in
    numberedTopRow :: numberedBoardRows


{-| Folds board row by row
-}
toIndexedList : Board square -> List ( Position, square )
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


size : Int
size =
    8


upperBound : Board e -> Position
upperBound (CreateBoard board) =
    let
        upper =
            Array.length board
    in
    ( upper, upper )


{-|

    Hardcoded since the board is fixed anyway

-}
inBounds : Position -> Bool
inBounds coord =
    isWithinRange coord ( size, size )


isWithinRange : Position -> Position -> Bool
isWithinRange ( c1, c2 ) ( c3, c4 ) =
    c1 < c3 && c2 < c4 && c1 >= 0 && c2 >= 0


positions : List Position
positions =
    let
        row column =
            List.range 0 7
                |> List.map (\r -> Position.make { x = r, y = column })
    in
    List.range 0 7
        |> List.concatMap row


get : Board square -> Position -> Maybe square
get (CreateBoard board) position =
    let
        getY =
            Array.get (Position.getY position) board

        getX =
            Array.get (Position.getX position)
    in
    getY
        |> Maybe.andThen getX


set : ( Position, square ) -> Board square -> Board square
set ( position, square ) (CreateBoard board) =
    let
        setRow =
            -- First extract from column
            Array.get (Position.getY position) board
                |> Maybe.map (Array.set (Position.getX position) square)

        setColumn row =
            Array.set (Position.getY position) row board
                |> CreateBoard
    in
    setRow
        |> Maybe.map setColumn
        |> Maybe.withDefault (CreateBoard board)


update : List ( Position, square ) -> Board square -> Board square
update changes board =
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
