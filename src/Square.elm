module Square exposing
    ( Square(..)
    , apply
    , applyIfOwner
    , belongs
    , blank
    , canSwapLeft
    , canSwapRight
    , check
    , checkPiece
    , collision
    , isOpponentOf
    , placeAll
    , toMaybe
    , updatePiece
    , view
    )

import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Highlight exposing (Highlight)
import Html exposing (Html)
import Html.Events as Html
import Piece exposing (Piece(..))
import Player exposing (Player(..))
import Position exposing (Position)
import Predicate exposing (Predicate)


type Square
    = Empty
    | Contains Player Piece


view : { size : Int, color : Highlight, square : Square, onClick : msg } -> Element msg
view { size, color, square, onClick } =
    case square of
        Empty ->
            Input.button
                [ Highlight.setBackground color
                , Border.solid
                , Border.width 2
                , Element.height
                    (Element.fill
                        |> Element.maximum size
                        |> Element.minimum size
                    )
                , Element.width
                    (Element.fill
                        |> Element.maximum size
                        |> Element.minimum size
                    )
                ]
                { onPress = Just onClick, label = Element.text " " }

        Contains player piece ->
            Input.button
                [ Highlight.setBackground color
                , Font.center
                , Border.solid
                , Border.width 2
                , Element.height
                    (Element.fill
                        |> Element.maximum size
                        |> Element.minimum size
                    )
                , Element.width
                    (Element.fill
                        |> Element.maximum size
                        |> Element.minimum size
                    )
                ]
                { onPress = Just onClick, label = Piece.view player piece }


toMaybe : Square -> Maybe ( Player, Piece )
toMaybe square =
    case square of
        Empty ->
            Nothing

        Contains player piece ->
            Just ( player, piece )


updatePiece : Square -> Square
updatePiece square =
    case square of
        Empty ->
            Empty

        Contains player piece ->
            Contains player (Piece.update piece)


applyIfOwner : Player -> (Player -> Piece -> a) -> Square -> Maybe a
applyIfOwner player f square =
    case square of
        Contains owner piece ->
            if owner == player then
                Just (f owner piece)

            else
                Nothing

        Empty ->
            Nothing


apply : (Player -> Piece -> a) -> Square -> Maybe a
apply f square =
    case square of
        Contains player piece ->
            Just (f player piece)

        Empty ->
            Nothing



-- PREDICATES


isOpponentOf : Player -> Square -> Bool
isOpponentOf player square =
    case square of
        Empty ->
            False

        Contains owner _ ->
            owner /= player


belongs : Player -> Square -> Bool
belongs player square =
    case square of
        Empty ->
            False

        Contains owner _ ->
            player == owner


blank : Square -> Bool
blank square =
    case square of
        Empty ->
            True

        Contains _ _ ->
            False


isKing : Player -> Square -> Bool
isKing player square =
    case square of
        Empty ->
            False

        Contains owner piece ->
            Piece.isKing piece && owner == player


collision : Square -> Bool
collision =
    not << blank


canSwapRight : (Position -> Maybe Square) -> Player -> Predicate Position
canSwapRight fromBoard player ( x, y ) =
    let
        blankSide =
            List.all (check fromBoard blank) [ ( x + 1, y ), ( x + 2, y ), ( x + 3, y ) ]

        isUnmovedRook =
            checkPiece
                fromBoard
                ((==) player)
                Piece.isUnmovedRook
                ( x + 4, y )

        isUnmovedKing =
            checkPiece
                fromBoard
                ((==) player)
                Piece.isUnmovedKing
                ( x, y )
    in
    blankSide && isUnmovedRook && isUnmovedKing


canSwapLeft : (Position -> Maybe Square) -> Player -> Predicate Position
canSwapLeft getSquare player ( x, y ) =
    let
        blankSide =
            List.all (check getSquare blank) [ ( x - 1, y ), ( x - 2, y ) ]

        isUnmovedRook =
            checkPiece
                getSquare
                ((==) player)
                Piece.isUnmovedRook
                ( x - 3, y )

        isUnmovedKing =
            checkPiece
                getSquare
                ((==) player)
                Piece.isUnmovedKing
                ( x, y )
    in
    blankSide && isUnmovedRook && isUnmovedKing


check : (Position -> Maybe Square) -> Predicate Square -> Predicate Position
check getSquare condition position =
    getSquare position
        |> Maybe.map (Predicate.check condition)
        |> Maybe.withDefault False


checkPiece :
    (Position -> Maybe Square)
    -> Predicate Player
    -> Predicate Piece
    -> Predicate Position
checkPiece getSquare playerCondition pieceCondition position =
    let
        checkSquare square =
            square
                |> toMaybe
                |> Maybe.map
                    (\( player, piece ) ->
                        Predicate.check playerCondition player
                            && Predicate.check pieceCondition piece
                    )
                |> Maybe.withDefault False
    in
    getSquare position
        |> Maybe.map checkSquare
        |> Maybe.withDefault False



-- BOARD


placeAll : List (List Square)
placeAll =
    let
        wPa =
            Contains White (Piece.Pawn False)

        wKn =
            Contains White Piece.Knight

        wKi =
            Contains White (Piece.King False)

        wRo =
            Contains White (Piece.Rook False)

        wBi =
            Contains White Piece.Bishop

        wQu =
            Contains White Piece.Queen

        bPa =
            Contains Black (Piece.Pawn False)

        bKn =
            Contains Black Piece.Knight

        bKi =
            Contains Black (Piece.King False)

        bRo =
            Contains Black (Piece.Rook False)

        bBi =
            Contains Black Piece.Bishop

        bQu =
            Contains Black Piece.Queen

        ooo =
            Empty
    in
    [ [ wRo, wKn, wBi, wKi, wQu, wBi, wKn, wRo ]
    , [ wPa, wPa, wPa, wPa, wPa, wPa, wPa, wPa ]
    , [ ooo, ooo, ooo, ooo, ooo, ooo, ooo, ooo ]
    , [ ooo, ooo, ooo, ooo, ooo, ooo, ooo, ooo ]
    , [ ooo, ooo, ooo, ooo, ooo, ooo, ooo, ooo ]
    , [ ooo, ooo, ooo, ooo, ooo, ooo, ooo, ooo ]
    , [ bPa, bPa, bPa, bPa, bPa, bPa, bPa, bPa ]
    , [ bRo, bKn, bBi, bKi, bQu, bBi, bKn, bRo ]
    ]
