module Main exposing (main)

import Board exposing (Board)
import Browser
import Browser.Dom as Dom
import Browser.Events as Events
import ChessBoard exposing (ChessBoard)
import Element exposing (Element)
import Highlight exposing (Highlight)
import Move exposing (Move(..))
import Move.Ruleset as Ruleset exposing (Ruleset, Valid)
import Piece exposing (Piece)
import Player exposing (Player)
import Position exposing (Position)
import Square exposing (Square)
import Task exposing (Task)


getSize : Task x ( Int, Int )
getSize =
    Dom.getViewport
        |> Task.map .viewport
        |> Task.map
            (\{ width, height } -> ( round width, round height ))


type Msg
    = Click Position
    | Resize ( Int, Int )


type alias Model =
    { current : Player
    , board : ChessBoard
    , message : String
    , selected : Maybe ChessBoard.Selected
    , size : Int
    }


type Loader
    = Loaded Model
    | Failed


load : Loader
load =
    let
        initial =
            ChessBoard.init
    in
    case initial of
        Just board ->
            Loaded
                { current = Player.White
                , board = board
                , message = "Welcome!"
                , selected = Nothing
                , size = 700
                }

        Nothing ->
            Failed


init : ( Loader, Cmd Msg )
init =
    ( load, Task.perform Resize getSize )


makeChanges :
    Model
    -> ChessBoard
    -> Model
makeChanges model board =
    { current = Player.next model.current
    , board = board
    , message = "Performed move!"
    , selected = Nothing
    , size = model.size
    }


setSize : Model -> ( Int, Int ) -> Model
setSize model ( x, y ) =
    { model | size = min x y }


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
update msg loader =
    case ( msg, loader ) of
        ( Click position, Loaded model ) ->
            ( Loaded (click position model), Cmd.none )

        ( Resize size, Loaded model ) ->
            ( Loaded (setSize model size), Cmd.none )

        ( _, Failed ) ->
            ( loader, Cmd.none )


highlight : Maybe ChessBoard.Selected -> Position -> Highlight
highlight selected position =
    case selected of
        Just { move } ->
            Move.highlight (move position)

        Nothing ->
            Highlight.None


viewGame : Model -> Element Msg
viewGame model =
    Element.wrappedRow [ Element.centerX ]
        [ Board.view
            { renderSquare =
                \position length square msg ->
                    Square.view
                        { square = square
                        , onClick = msg
                        , color = highlight model.selected position
                        , size = length
                        }
            , indexedMsg = Click
            , board = model.board
            , length = model.size
            }
        , Element.column [ Element.width (Element.px 500) ]
            [ Player.view model.current
            , Element.text
                model.message
            ]
        ]


errorMessage : String -> Element Msg
errorMessage message =
    Element.text message


view : Loader -> Browser.Document Msg
view model =
    case model of
        Loaded data ->
            { title = "Elm Chess!"
            , body = [ Element.layout [] (viewGame data) ]
            }

        Failed ->
            { title = "Loading failed"
            , body = [ Element.layout [] (errorMessage "Malformed chess board") ]
            }


handleResize : Sub Msg
handleResize =
    Events.onResize (\x y -> Resize ( x, y ))


main : Program () Loader Msg
main =
    Browser.document
        { init = \_ -> init
        , update = update
        , view = view
        , subscriptions =
            \_ -> handleResize
        }
