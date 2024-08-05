module Main exposing (..)

import Browser
import GameLoop exposing (Msg)
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type CellState
    = Empty
    | X
    | O


cellToString : CellState -> String
cellToString cell =
    case cell of
        Empty ->
            " "

        X ->
            "X"

        O ->
            "O"


type alias Model =
    { turn : CellState
    , timewarp : List (List CellState)
    , board : List CellState
    }


init : Model
init =
    Model X [ List.repeat 9 Empty ] (List.repeat 9 Empty)



-- UPDATE


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            { model | turn = O }

        Decrement ->
            { model | turn = X }



-- VIEW


view : Model -> Html Msg
view model =
    div [ style "border" "1px solid #0a0305" ]
        [ div []
            (List.map (\cell -> div [] [ text (cellToString cell) ]) model.board)
        ]
