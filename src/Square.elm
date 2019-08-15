module Square exposing (Square(..), initialBoard)

import Board exposing (Board)
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


type alias Coordinate =
    ( Int, Int )


display : Square -> String
display square =
    case square of
        Blank ->
            " "

        IsPiece player piece ->
            Piece.show player piece


type Square
    = Blank
    | IsPiece Player Piece


belongs : Board Square -> Player -> Coordinate -> Bool
belongs board player coordinate =
    Board.get board coordinate
        |> Maybe.map
            (\square ->
                case square of
                    Blank ->
                        False

                    IsPiece owner _ ->
                        player == owner
            )
        |> Maybe.withDefault False


isBlank : Board Square -> Coordinate -> Bool
isBlank board coordinate =
    Board.get board coordinate
        |> Maybe.map
            (\square ->
                case square of
                    Blank ->
                        True

                    IsPiece _ _ ->
                        False
            )
        |> Maybe.withDefault False


isCollision : Board Square -> Coordinate -> Bool
isCollision board coordinate =
    Board.get board coordinate
        |> Maybe.map
            (\square ->
                case square of
                    Blank ->
                        False

                    IsPiece _ _ ->
                        True
            )
        |> Maybe.withDefault False


extractPlayer : Square -> Maybe Player
extractPlayer square =
    case square of
        Blank ->
            Nothing

        IsPiece player _ ->
            Just player


extractSquare : Square -> Maybe ( Player, Piece )
extractSquare square =
    case square of
        Blank ->
            Nothing

        IsPiece player piece ->
            Just ( player, piece )


isOpponent : Player -> Square -> Bool
isOpponent player square =
    extractPlayer square
        |> Maybe.map (\owner -> owner /= player)
        |> Maybe.withDefault False


squareBelongingTo : Player -> Square -> Maybe ( Player, Piece )
squareBelongingTo player square =
    extractSquare square
        |> Maybe.andThen
            (\( owner, piece ) ->
                if player == owner then
                    Just ( player, piece )

                else
                    Nothing
            )


check : Board Square -> Coordinate -> (Square -> Bool) -> Bool
check board coord condition =
    Board.get board coord
        |> Maybe.map condition
        |> Maybe.withDefault False


checkPiece : Board Square -> Coordinate -> (( Player, Piece ) -> Bool) -> Bool
checkPiece board coord condition =
    Board.get board coord
        |> Maybe.andThen extractSquare
        |> Maybe.map condition
        |> Maybe.withDefault False


swapRight : Board Square -> Player -> Coordinate -> Bool
swapRight board player ( x, y ) =
    let
        blankSide =
            List.all (isBlank board) [ ( x + 1, y ), ( x + 2, y ) ]

        isUnmovedRook =
            checkPiece board
                ( x + 3, y )
                (\( owner, piece ) ->
                    owner == player && Piece.isUnmovedRook piece
                )

        isUnmovedKing =
            checkPiece board
                ( x, y )
                (\( owner, piece ) ->
                    owner == player && Piece.isUnmovedKing piece
                )
    in
    blankSide && isUnmovedRook && isUnmovedKing


swapLeft : Board Square -> Player -> Coordinate -> Bool
swapLeft board player ( x, y ) =
    let
        blankSide =
            List.all (isBlank board) [ ( x - 1, y ), ( x - 2, y ), ( x - 3, y ) ]

        isUnmovedRook =
            checkPiece board
                ( x - 4, y )
                (\( owner, piece ) ->
                    owner == player && Piece.isUnmovedRook piece
                )

        isUnmovedKing =
            checkPiece board
                ( x, y )
                (\( owner, piece ) ->
                    owner == player && Piece.isUnmovedKing piece
                )
    in
    blankSide && isUnmovedRook && isUnmovedKing


pieceThreatened : Board Square -> Player -> Coordinate -> Bool
pieceThreatened board player position =
    let
        opponent =
            Player.next player

        withCoordinate coordinate maybeSquare =
            Maybe.map (\square -> ( coordinate, square )) maybeSquare

        isInRangeOf enemyMoves =
            Moves.rules enemyMoves
                |> Moves.member position

        opponentsRange =
            Board.toIndexedList board
                |> List.filterMap
                    (\( coordinate, square ) ->
                        squareBelongingTo opponent square
                            |> withCoordinate coordinate
                    )
                |> List.filterMap (\( coordinate, _ ) -> moves board coordinate)
    in
    isInRangeOf opponentsRange



-- Predicates


belongsPredicate : Board Square -> Player -> Predicate Coordinate BelongsToPlayer
belongsPredicate board player =
    Predicate.make (belongs board player) Predicate.BelongsToPlayer


outOfBoundsPredicate : Predicate Coordinate OutOfBounds
outOfBoundsPredicate =
    Predicate.make Board.inBounds Predicate.OutOfBounds


collisionPredicate : Board Square -> Predicate Coordinate IsCollision
collisionPredicate board =
    Predicate.make (isCollision board) Predicate.IsCollision


threatPredicate : Board Square -> Player -> Predicate Coordinate Threatened
threatPredicate board player =
    Predicate.make (pieceThreatened board player) Predicate.Threatened


blankPredicate : Board Square -> Predicate Coordinate IsBlank
blankPredicate board =
    Predicate.make (isBlank board) Predicate.IsBlank


swapRightPredicate : Board Square -> Player -> Predicate Coordinate Swappable
swapRightPredicate board player =
    Predicate.make (swapRight board player) Predicate.Swappable


swapLeftPredicate : Board Square -> Player -> Predicate Coordinate Swappable
swapLeftPredicate board player =
    Predicate.make (swapLeft board player) Predicate.Swappable



-- BOARD


movesHelper :
    { board : Board Square
    , coordinate : Coordinate
    , player : Player
    , piece : Piece
    }
    -> Moves Valid
movesHelper { board, coordinate, player, piece } =
    case piece of
        Pawn hasMoved ->
            Moves.pawn
                { player = player
                , belongsToPlayer = belongsPredicate board player
                , isBlank = blankPredicate board
                , collision = collisionPredicate board
                , outOfBounds = outOfBoundsPredicate
                , hasMoved = hasMoved
                , coord = coordinate
                }

        Rook _ ->
            Moves.rook
                { belongsToPlayer = belongsPredicate board player
                , outOfBounds = outOfBoundsPredicate
                , coord = coordinate
                }

        King _ ->
            Moves.king
                { belongsToPlayer = belongsPredicate board player
                , outOfBounds = outOfBoundsPredicate
                , isThreatened = threatPredicate board player
                , coord = coordinate
                , swapRight = swapRightPredicate board player
                , swapLeft = swapLeftPredicate board player
                }

        Queen ->
            Moves.queen
                { belongsToPlayer = belongsPredicate board player
                , outOfBounds = outOfBoundsPredicate
                , coord = coordinate
                }

        Bishop ->
            Moves.bishop
                { belongsToPlayer = belongsPredicate board player
                , outOfBounds = outOfBoundsPredicate
                , coord = coordinate
                }

        Knight ->
            Moves.knight
                { belongsToPlayer = belongsPredicate board player
                , outOfBounds = outOfBoundsPredicate
                , coord = coordinate
                }


moves : Board Square -> Coordinate -> Maybe (Moves Valid)
moves board coordinate =
    Board.get board coordinate
        |> Maybe.andThen extractSquare
        |> Maybe.map
            (\( player, piece ) ->
                movesHelper
                    { board = board
                    , player = player
                    , piece = piece
                    , coordinate = coordinate
                    }
            )


initialBoard : Maybe (Board Square)
initialBoard =
    let
        wPa =
            IsPiece White (Piece.Pawn False)

        wKn =
            IsPiece White Piece.Knight

        wKi =
            IsPiece White (Piece.King False)

        wRo =
            IsPiece White (Piece.Rook False)

        wBi =
            IsPiece White Piece.Bishop

        wQu =
            IsPiece White Piece.Queen

        bPa =
            IsPiece Black (Piece.Pawn False)

        bKn =
            IsPiece Black Piece.Knight

        bKi =
            IsPiece Black (Piece.King False)

        bRo =
            IsPiece Black (Piece.Rook False)

        bBi =
            IsPiece Black Piece.Bishop

        bQu =
            IsPiece Black Piece.Queen

        bla =
            Blank
    in
    [ [ wRo, wKn, wBi, wKi, wQu, wBi, wKn, wRo ]
    , [ wPa, wPa, wPa, wPa, wPa, wPa, wPa, wPa ]
    , [ bla, bla, bla, bla, bla, bla, bla, bla ]
    , [ bla, bla, bla, bla, bla, bla, bla, bla ]
    , [ bla, bla, bla, bla, bla, bla, bla, bla ]
    , [ bla, bla, bla, bla, bla, bla, bla, bla ]
    , [ bPa, bPa, bPa, bPa, bPa, bPa, bPa, bPa ]
    , [ bRo, bKn, bBi, bKi, bQu, bBi, bKn, bRo ]
    ]
        |> Board.construct
