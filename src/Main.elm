module Main exposing (Data, Model(..), Msg(..), Position, init, load, main, makeChanges, update, view)

import Board exposing (Board)
import Browser
import Element exposing (Element)
import Player exposing (Player)
import Square exposing (Square)


type Msg
    = Click Position


type alias Position =
    ( Int, Int )


type alias Data =
    { current : Player
    , board : Board Square
    , message : String
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
                }

        Nothing ->
            Failed


init : ( Model, Cmd Msg )
init =
    ( load, Cmd.none )


makeChanges : Data -> List ( Position, Square ) -> Data
makeChanges model changes =
    { current = Player.next model.current
    , board = Board.update model.board changes
    , message = "Performed move!"
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update (Click ( x, y )) model =
    case model of
        Loaded data ->
            ( Loaded { data | message = "Clicked: " ++ String.fromInt x }, Cmd.none )

        Failed ->
            ( model, Cmd.none )


viewHelper : Data -> Element Msg
viewHelper model =
    Element.column []
        [ Board.view Square.view Click model.board
        , Player.display model.current
        , Element.text model.message
        ]


errorMessage : String -> Element Msg
errorMessage =
    Element.text


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
