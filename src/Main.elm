module Main exposing (Data, Model(..), Msg(..), init, load, main, makeChanges, update, view)

import Board exposing (Board)
import Browser
import Element exposing (Element)
import Moves exposing (Moves, Valid)
import Piece exposing (Piece)
import Player exposing (Player)
import Position exposing (Position)
import Square exposing (Square)


type Msg
    = Click Position


type alias Data =
    { current : Player
    , board : Board Square
    , message : String
    , selected : Maybe ( Position, Square, Moves Valid )
    }


type Model
    = Loaded Data
    | Failed


load : Model
load =
    let
        initialData =
            Square.initialBoard
                |> Maybe.map
                    (\board ->
                        { current = Player.White
                        , board = board
                        }
                    )
    in
    case initialData of
        Just state ->
            Loaded
                { current = state.current
                , board = state.board
                , message = "Welcome!"
                , selected = Nothing
                }

        Nothing ->
            Failed


init : ( Model, Cmd Msg )
init =
    ( load, Cmd.none )


makeChanges : Data -> List ( Position, Square ) -> Data
makeChanges model changes =
    { current = Player.next model.current
    , board = Board.update changes model.board
    , message = "Performed move!"
    , selected = Nothing
    }


select : Position -> Data -> Data
select position model =
    Square.select model.board position model.current
        |> Maybe.map
            (\( piece, moves ) ->
                { model
                    | selected = Just ( position, piece, moves )
                    , message =
                        "Selected: "
                            ++ String.fromInt (Position.getX position)
                            ++ " "
                            ++ String.fromInt (Position.getY position)
                }
            )
        |> Maybe.withDefault
            { model
                | message =
                    "Selected: "
                        ++ String.fromInt (Position.getX position)
                        ++ " "
                        ++ String.fromInt (Position.getY position)
            }


click : Position -> Data -> Data
click position model =
    case model.selected of
        Just ( piecePosition, pieceType, availableMoves ) ->
            if Moves.member position availableMoves then
                makeChanges model
                    [ ( position, Square.updatePiece pieceType )
                    , ( piecePosition, Square.Blank )
                    ]

            else
                { model | selected = Nothing }
                    |> select position

        Nothing ->
            select position model


update : Msg -> Model -> ( Model, Cmd Msg )
update (Click position) model =
    case model of
        Loaded data ->
            ( Loaded (click position data), Cmd.none )

        Failed ->
            ( model, Cmd.none )


highlighter : Maybe ( Position, Square, Moves Valid ) -> Position -> Bool
highlighter selected position =
    case selected of
        Just ( _, _, highlightSet ) ->
            Moves.member position highlightSet

        Nothing ->
            False


viewHelper : Data -> Element Msg
viewHelper model =
    Element.column []
        [ Board.view
            { renderSquare =
                \position square msg ->
                    Square.view
                        { square = square
                        , onClick = msg
                        , highlight = highlighter model.selected position
                        }
            , indexedMsg = Click
            , board = model.board
            }
        , Player.display model.current
        , Element.text model.message
        ]


errorMessage : String -> Element Msg
errorMessage message =
    Element.text message


view : Model -> Browser.Document Msg
view model =
    case model of
        Loaded data ->
            { title = "Elm Chess!"
            , body = [ Element.layout [] (viewHelper data) ]
            }

        Failed ->
            { title = "Loading failed"
            , body = [ Element.layout [] (errorMessage "Malformed chess board") ]
            }


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
