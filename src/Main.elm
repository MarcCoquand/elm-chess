module Main exposing (Loader(..), Model, Msg(..), click, errorMessage, highlighter, init, load, main, makeChanges, select, update, view, viewHelper)

import Board exposing (Board)
import Browser
import ChessBoard exposing (ChessBoard)
import Element exposing (Element)
import Move exposing (Move(..))
import Piece exposing (Piece)
import Player exposing (Player)
import Position exposing (Position)
import Square exposing (Square)


type Msg
    = Click Position


type alias Model =
    { current : Player
    , board : ChessBoard
    , message : String
    , selected : Maybe ChessBoard.Selected
    }


type Loader
    = Loaded Model
    | Failed


load : Loader
load =
    let
        initialModel =
            ChessBoard.init
                |> Maybe.map
                    (\board ->
                        { current = Player.White
                        , board = board
                        }
                    )
    in
    case initialModel of
        Just state ->
            Loaded
                { current = state.current
                , board = state.board
                , message = "Welcome!"
                , selected = Nothing
                }

        Nothing ->
            Failed


init : ( Loader, Cmd Msg )
init =
    ( load, Cmd.none )


makeChanges :
    Model
    -> ChessBoard
    -> Model
makeChanges model board =
    { current = Player.next model.current
    , board = board
    , message = "Performed move!"
    , selected = Nothing
    }


select : Position -> Model -> Model
select position model =
    { model
        | selected = ChessBoard.select model.board model.current position
        , message =
            "Choose where to move."
    }


click : Position -> Model -> Model
click position model =
    case model.selected of
        Just selected ->
            ChessBoard.performMove model.board (selected.move position)
                |> Maybe.map (\board -> makeChanges model board)
                |> Maybe.withDefault
                    ({ model | selected = Nothing }
                        |> select position
                    )

        Nothing ->
            select position model


update : Msg -> Loader -> ( Loader, Cmd Msg )
update (Click position) model =
    case model of
        Loaded data ->
            ( Loaded (click position data), Cmd.none )

        Failed ->
            ( model, Cmd.none )


highlighter : Maybe ChessBoard.Selected -> Position -> Bool
highlighter selected position =
    case selected of
        Just { move } ->
            Move.isValid (move position)

        Nothing ->
            False


viewHelper : Model -> Element Msg
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
        , Player.view model.current
        , Element.text model.message
        ]


errorMessage : String -> Element Msg
errorMessage message =
    Element.text message


view : Loader -> Browser.Document Msg
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


main : Program () Loader Msg
main =
    Browser.document
        { init = \_ -> init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
