module Square exposing (Get, Square(..), belongs, blank, blue, check, checkPiece, collision, extractPlayer, get, getBelongingTo, highlighter, isOpponent, pieceThreatened, swapLeft, swapRight, updatePiece, view, white)

import Board exposing (Board)
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Html exposing (Html)
import Html.Events as Html
import Moves exposing (Moves, Valid)
import Piece exposing (Piece(..))
import Player exposing (Player(..))
import Position exposing (Position)
import Predicate exposing (Predicate)


type Square
    = Blank
    | Contains Player Piece


type alias Get =
    Position -> Maybe Square


white =
    Element.rgb255 255 255 255


blue =
    Element.rgb255 200 200 255


highlighter enabled =
    if enabled then
        Background.color blue

    else
        Background.color white


view : { highlight : Bool, square : Square, onClick : msg } -> Element msg
view { highlight, square, onClick } =
    case square of
        Blank ->
            Input.button
                [ highlighter highlight
                , Border.solid
                , Border.width 2
                ]
                { onPress = Just onClick, label = Element.text " " }

        Contains player piece ->
            Input.button [ highlighter highlight, Border.solid, Border.width 2 ]
                { onPress = Just onClick, label = Piece.view player piece }


extractPlayer : Square -> Maybe Player
extractPlayer square =
    case square of
        Blank ->
            Nothing

        Contains player _ ->
            Just player


get : Square -> Maybe ( Player, Piece )
get square =
    case square of
        Blank ->
            Nothing

        Contains player piece ->
            Just ( player, piece )


isOpponent : Player -> Square -> Bool
isOpponent player square =
    extractPlayer square
        |> Maybe.map (\owner -> owner /= player)
        |> Maybe.withDefault False


getBelongingTo : Player -> Square -> Maybe ( Player, Piece )
getBelongingTo player square =
    get square
        |> Maybe.andThen
            (\( owner, piece ) ->
                if player == owner then
                    Just ( player, piece )

                else
                    Nothing
            )


updatePiece : Square -> Square
updatePiece square =
    case square of
        Blank ->
            Blank

        Contains player piece ->
            Contains player (Piece.update piece)



-- CONDITIONS


belongs : Player -> Square -> Bool
belongs player square =
    case square of
        Blank ->
            False

        Contains owner _ ->
            player == owner


blank : Square -> Bool
blank square =
    case square of
        Blank ->
            True

        Contains _ _ ->
            False


collision : Square -> Bool
collision square =
    case square of
        Blank ->
            False

        Contains _ _ ->
            True


check : (Square -> Bool) -> (Position -> Maybe Square) -> Position -> Bool
check condition getter position =
    getter position
        |> Maybe.map condition
        |> Maybe.withDefault False


checkPiece : { getter : Get, position : Position, condition : ( Player, Piece ) -> Bool } -> Bool
checkPiece { getter, position, condition } =
    getter position
        |> Maybe.andThen get
        |> Maybe.map condition
        |> Maybe.withDefault False


swapRight : Get -> Player -> Position -> Bool
swapRight getter player ( x, y ) =
    let
        blankSide =
            List.all (check blank getter) [ ( x + 1, y ), ( x + 2, y ) ]

        isUnmovedRook =
            checkPiece
                { getter = getter
                , position = ( x + 3, y )
                , condition =
                    \( owner, piece ) ->
                        owner == player && Piece.isUnmovedRook piece
                }

        isUnmovedKing =
            checkPiece
                { getter = getter
                , position = ( x, y )
                , condition =
                    \( owner, piece ) ->
                        owner == player && Piece.isUnmovedKing piece
                }
    in
    blankSide && isUnmovedRook && isUnmovedKing


swapLeft : Get -> Player -> Position -> Bool
swapLeft getter player ( x, y ) =
    let
        blankSide =
            List.all (check blank getter) [ ( x - 1, y ), ( x - 2, y ), ( x - 3, y ) ]

        isUnmovedRook =
            checkPiece
                { getter = getter
                , position = ( x - 4, y )
                , condition =
                    \( owner, piece ) ->
                        owner == player && Piece.isUnmovedRook piece
                }

        isUnmovedKing =
            checkPiece
                { getter = getter
                , position = ( x, y )
                , condition =
                    \( owner, piece ) ->
                        owner == player && Piece.isUnmovedKing piece
                }
    in
    blankSide && isUnmovedRook && isUnmovedKing


pieceThreatened : Board Square -> Player -> Position -> Bool
pieceThreatened board player position =
    let
        opponent =
            Player.next player

        withPosition boardPosition maybeSquare =
            Maybe.map (\square -> ( boardPosition, square )) maybeSquare

        isInRangeOf enemyMoves =
            Moves.rules enemyMoves
                |> Moves.member position

        opponentsRange =
            Board.toIndexedList board
                |> List.filterMap
                    (\( boardPosition, square ) ->
                        getBelongingTo opponent square
                            |> withPosition boardPosition
                    )
                |> List.filterMap
                    (\( boardPosition, _ ) ->
                        moves board
                            boardPosition
                    )
    in
    isInRangeOf opponentsRange
