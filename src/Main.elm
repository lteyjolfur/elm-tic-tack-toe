module Main exposing (..)

import Browser
import Debug exposing (log)
import GameLoop exposing (Msg)
import Html exposing (Html, button, div, h1, h2, text)
import Html.Attributes exposing (disabled, style)
import Html.Events exposing (onClick)
import List.Extra exposing (..)



-- IDEAS
-- Custom size
-- Sounds?
-- StyleSheet
-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


boolToString : Bool -> String
boolToString b =
    if b then
        "true"

    else
        "false"


cellSize : Int
cellSize =
    30


cellWidth : Int
cellWidth =
    3


type CellState
    = Empty
    | X
    | O


type GameState
    = Playing
    | GameOver


type TimeDirection
    = Backward
    | Forward


cellToString : CellState -> String
cellToString currentCell =
    case currentCell of
        Empty ->
            "-"

        X ->
            "X"

        O ->
            "O"


makeWinCons : List (List Int)
makeWinCons =
    let
        emptyLength =
            List.range 0 (cellWidth - 1)

        rows =
            List.indexedMap (\i _ -> List.range (i * cellWidth) (i * cellWidth + cellWidth - 1)) emptyLength

        cols =
            List.indexedMap (\i _ -> List.map (\j -> i + j * cellWidth) emptyLength) emptyLength

        lDiagonal =
            [ List.map (\c -> c + c * 3) emptyLength ]

        rDiagonal =
            [ List.map (\c -> c * 3 - c - 1 + cellWidth) emptyLength ]

        winCons =
            rows ++ cols ++ lDiagonal ++ rDiagonal
    in
    log "yay" winCons


getIsWinSlice : List CellState -> Bool
getIsWinSlice slice =
    if List.all (\s -> X == s) slice then
        True

    else if List.all (\s -> O == s) slice then
        True

    else
        False


checkWin :
    List CellState
    -> List (List Int)
    -> List Bool
checkWin board winCons =
    let
        boardSlices =
            List.map (List.map (\i -> getCellStateAt i board)) winCons

        isWinList =
            List.map (\s -> getIsWinSlice s) boardSlices
    in
    log "isWinList" isWinList



--++ diagonals)


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


getCellStateAt : Int -> List CellState -> CellState
getCellStateAt index board =
    Maybe.withDefault O (List.Extra.getAt index board)


getWinConAt : Int -> List (List Int) -> List Int
getWinConAt index winCons =
    Maybe.withDefault [] (List.Extra.getAt index winCons)


type alias Model =
    { turn : CellState
    , timewarp : List (List CellState)
    , board : List CellState
    , gameState : GameState
    , winCons : List (List Int)
    , winSlice : List Int
    , timewarpTurn : Int

    --, winner : CellState
    }


init : Model
init =
    Model X
        [ [] ]
        (List.repeat 9 Empty)
        Playing
        makeWinCons
        []
        0



--Empty
-- UPDATE


type Msg
    = Play Int
    | Restart
    | Timewarp TimeDirection


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

                isWinList =
                    checkWin newBoard model.winCons

                isWin =
                    List.any (\s -> s == True) isWinList

                newGameState =
                    if isWin then
                        GameOver

                    else
                        Playing

                newTimeWarpTurn =
                    if newGameState == GameOver then
                        List.length model.timewarp

                    else
                        0

                newTurn =
                    if getCellStateAt index model.board == Empty && List.all (\s -> s == False) isWinList then
                        toggleTurn model.turn

                    else
                        model.turn

                newWinSlice =
                    isWinList
                        |> List.indexedMap Tuple.pair
                        |> List.filter (\( _, s ) -> True == s)
                        |> List.map Tuple.first
                        |> log "newWinSlice"
                        |> List.head
                        |> Maybe.withDefault 100
            in
            if model.gameState == GameOver then
                model

            else
                { model
                    | turn = newTurn
                    , board = newBoard
                    , winSlice = getWinConAt newWinSlice model.winCons
                    , gameState = newGameState
                    , timewarp = model.timewarp ++ [ newBoard ]
                    , timewarpTurn = newTimeWarpTurn
                }

        Restart ->
            init

        Timewarp direction ->
            if direction == Forward then
                { model | board = List.Extra.getAt (model.timewarpTurn + 1) model.timewarp |> Maybe.withDefault model.board, timewarpTurn = model.timewarpTurn + 1 }

            else
                { model | board = List.Extra.getAt (model.timewarpTurn - 1) model.timewarp |> Maybe.withDefault model.board, timewarpTurn = model.timewarpTurn - 1 }



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
        , h2 []
            [ text
                (if model.gameState == GameOver then
                    String.concat [ "Winner: ", cellToString model.turn, "!" ]

                 else
                    cellToString model.turn
                )
            ]
        , div
            [ style "border" "0.5px solid #0a0305"
            , style "border-radius" "3px"
            , style "width" "93px"
            , style "cursor"
                (if model.gameState == GameOver then
                    "default"

                 else
                    "pointer"
                )
            , style "margin" "0 auto"
            , style "margin-top" "40px"
            ]
            (List.indexedMap (\i c -> cell c i model.winSlice) model.board)

        -- , div []
        --     (List.map (\b -> div [] [ text (boolToString b) ]) (checkWin model.board model.winCons))
        , button
            [ onClick Restart
            , disabled (model.gameState /= GameOver)
            , style "margin" "24px 0 12px 0"
            ]
            [ text "Play again" ]
        , div []
            [ text "Timewarp" ]
        , div []
            [ button [ onClick (Timewarp Backward), disabled (model.gameState /= GameOver) ] [ text "<" ]
            , button [ onClick (Timewarp Forward), disabled (model.gameState /= GameOver) ] [ text ">" ]
            ]
        ]


cell : CellState -> Int -> List Int -> Html Msg
cell cState index winSlice =
    div
        [ style "border" "0.5px solid #0a0305"
        , style "width" (toPx cellSize)
        , style "height" (toPx cellSize)
        , style "display" "inline-block"
        , style "line-height" (toPx cellSize)
        , style "background-color" "#bab0b9"
        , style "border-radius" "3px"
        , style "color"
            (if cState == Empty then
                "transparent"

             else if List.any (\i -> i == index) winSlice then
                "red"

             else
                "inherit"
            )
        , onClick (Play index)
        ]
        [ text (cellToString cState) ]
