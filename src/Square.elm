module Square exposing (Square(..), initialBoard, view)

import Board exposing (Board)
import Element exposing (Element)
import Element.Background as Background
import Element.Input as Input
import Html exposing (Html)
import Html.Events as Html
import Moves exposing (Moves, Valid)
import Piece exposing (Piece(..))
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


type alias Position =
    ( Int, Int )


type Square
    = Blank
    | Contains Player Piece


show : Square -> String
show square =
    case square of
        Blank ->
            " "

        Contains player piece ->
            Piece.show player piece


white =
    Element.rgb255 255 255 255


view : Square -> msg -> Element msg
view square msg =
    case square of
        Blank ->
            Element.text " "

        Contains player piece ->
            Input.button [ Background.color white ]
                { onPress = Just msg, label = Piece.view player piece }


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


belongs : Board Square -> Player -> Position -> Bool
belongs board player position =
    Board.get board position
        |> Maybe.map
            (\square ->
                case square of
                    Blank ->
                        False

                    Contains owner _ ->
                        player == owner
            )
        |> Maybe.withDefault False


isBlank : Board Square -> Position -> Bool
isBlank board position =
    Board.get board position
        |> Maybe.map
            (\square ->
                case square of
                    Blank ->
                        True

                    Contains _ _ ->
                        False
            )
        |> Maybe.withDefault False


isCollision : Board Square -> Position -> Bool
isCollision board position =
    Board.get board position
        |> Maybe.map
            (\square ->
                case square of
                    Blank ->
                        False

                    Contains _ _ ->
                        True
            )
        |> Maybe.withDefault False


check : Board Square -> Position -> (Square -> Bool) -> Bool
check board coord condition =
    Board.get board coord
        |> Maybe.map condition
        |> Maybe.withDefault False


checkPiece : { board : Board Square, position : Position, condition : ( Player, Piece ) -> Bool } -> Bool
checkPiece { board, position, condition } =
    Board.get board position
        |> Maybe.andThen get
        |> Maybe.map condition
        |> Maybe.withDefault False


swapRight : Board Square -> Player -> Position -> Bool
swapRight board player ( x, y ) =
    let
        blankSide =
            List.all (isBlank board) [ ( x + 1, y ), ( x + 2, y ) ]

        isUnmovedRook =
            checkPiece
                { board = board
                , position = ( x + 3, y )
                , condition =
                    \( owner, piece ) ->
                        owner == player && Piece.isUnmovedRook piece
                }

        isUnmovedKing =
            checkPiece
                { board = board
                , position = ( x, y )
                , condition =
                    \( owner, piece ) ->
                        owner == player && Piece.isUnmovedKing piece
                }
    in
    blankSide && isUnmovedRook && isUnmovedKing


swapLeft : Board Square -> Player -> Position -> Bool
swapLeft board player ( x, y ) =
    let
        blankSide =
            List.all (isBlank board) [ ( x - 1, y ), ( x - 2, y ), ( x - 3, y ) ]

        isUnmovedRook =
            checkPiece
                { board = board
                , position = ( x - 4, y )
                , condition =
                    \( owner, piece ) ->
                        owner == player && Piece.isUnmovedRook piece
                }

        isUnmovedKing =
            checkPiece
                { board = board
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



-- Predicates


belongsPredicate : Board Square -> Player -> Predicate Position BelongsToPlayer
belongsPredicate board player =
    Predicate.make (belongs board player) Predicate.BelongsToPlayer


outOfBoundsPredicate : Predicate Position OutOfBounds
outOfBoundsPredicate =
    Predicate.make Board.inBounds Predicate.OutOfBounds


collisionPredicate : Board Square -> Predicate Position IsCollision
collisionPredicate board =
    Predicate.make (isCollision board) Predicate.IsCollision


threatPredicate : Board Square -> Player -> Predicate Position Threatened
threatPredicate board player =
    Predicate.make (pieceThreatened board player) Predicate.Threatened


blankPredicate : Board Square -> Predicate Position IsBlank
blankPredicate board =
    Predicate.make (isBlank board) Predicate.IsBlank


swapRightPredicate : Board Square -> Player -> Predicate Position Swappable
swapRightPredicate board player =
    Predicate.make (swapRight board player) Predicate.Swappable


swapLeftPredicate : Board Square -> Player -> Predicate Position Swappable
swapLeftPredicate board player =
    Predicate.make (swapLeft board player) Predicate.Swappable



-- BOARD


movesHelper :
    { board : Board Square
    , position : Position
    , player : Player
    , piece : Piece
    }
    -> Moves Valid
movesHelper { board, position, player, piece } =
    case piece of
        Pawn hasMoved ->
            Moves.pawn
                { player = player
                , belongsToPlayer = belongsPredicate board player
                , isBlank = blankPredicate board
                , collision = collisionPredicate board
                , outOfBounds = outOfBoundsPredicate
                , hasMoved = hasMoved
                , position = position
                }

        Rook _ ->
            Moves.rook
                { belongsToPlayer = belongsPredicate board player
                , outOfBounds = outOfBoundsPredicate
                , position = position
                }

        King _ ->
            Moves.king
                { belongsToPlayer = belongsPredicate board player
                , outOfBounds = outOfBoundsPredicate
                , isThreatened = threatPredicate board player
                , position = position
                , swapRight = swapRightPredicate board player
                , swapLeft = swapLeftPredicate board player
                }

        Queen ->
            Moves.queen
                { belongsToPlayer = belongsPredicate board player
                , outOfBounds = outOfBoundsPredicate
                , position = position
                }

        Bishop ->
            Moves.bishop
                { belongsToPlayer = belongsPredicate board player
                , outOfBounds = outOfBoundsPredicate
                , position = position
                }

        Knight ->
            Moves.knight
                { belongsToPlayer = belongsPredicate board player
                , outOfBounds = outOfBoundsPredicate
                , position = position
                }


moves : Board Square -> Position -> Maybe (Moves Valid)
moves board position =
    Board.get board position
        |> Maybe.andThen get
        |> Maybe.map
            (\( player, piece ) ->
                movesHelper
                    { board = board
                    , player = player
                    , piece = piece
                    , position = position
                    }
            )


initialBoard : Maybe (Board Square)
initialBoard =
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
            Blank
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
        |> Board.construct
