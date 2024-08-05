module Main exposing (..)

import Browser
import GameLoop exposing (Msg)
import Html exposing (Html, div, h1, h2, text)
import Html.Attributes exposing (style)
import Html.Events exposing (on, onClick)
import List exposing (indexedMap)



-- IDEAS
-- Custom size
-- Time travel
-- Sounds?
-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


cellSize : Int
cellSize =
    30


type CellState
    = Empty
    | X
    | O


cellToString : CellState -> String
cellToString currentCell =
    case currentCell of
        Empty ->
            "-"

        X ->
            "X"

        O ->
            "O"


toPx : Int -> String
toPx n =
    String.fromInt n ++ "px"


toggleTurn : CellState -> CellState
toggleTurn c =
    if c == X then
        O

    else
        X


matchCell : CellState -> CellState -> CellState
matchCell newCell boardCell =
    if boardCell == Empty then
        newCell

    else
        boardCell


type alias Model =
    { turn : CellState
    , timewarp : List (List CellState)
    , board : List CellState
    }


init : Model
init =
    Model X
        [ [] ]
        (List.repeat 9 Empty)



-- UPDATE


type Msg
    = Play Int
    | Toggle Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        Play index ->
            let
                newBoard =
                    List.indexedMap
                        (\i c ->
                            if i == index then
                                matchCell model.turn c

                            else
                                c
                        )
                        model.board
            in
            { model | turn = toggleTurn model.turn, board = newBoard }

        Toggle index ->
            model



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ style "box-sizing" "border-box"
        , style "text-align" "center"

        -- , style "display" "flex"
        -- , style "justify-items" "center"
        -- , style "align-items" "center"
        ]
        [ h1 [] [ text "Tic Tac Toe" ]
        , h2 [] [ text (cellToString model.turn) ]
        , div
            [ style "border" "0.5px solid #0a0305"
            , style "width" "93px"
            , style "cursor" "pointer"
            , style "margin" "0 auto"
            , style "margin-top" "40px"
            ]
            (List.indexedMap (\i c -> cell c i) model.board)
        ]


cell : CellState -> Int -> Html Msg
cell cState index =
    div
        [ style "border" "0.5px solid #0a0305"
        , style "width" (toPx cellSize)
        , style "height" (toPx cellSize)
        , style "display" "inline-block"
        , onClick (Play index)
        ]
        [ text (cellToString cState) ]
