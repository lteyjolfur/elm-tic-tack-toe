module MainTests exposing (suite)

import Expect
import Main exposing (CellState(..), GameState(..), Msg(..))
import Test exposing (Test, describe, test)


playAll : List Int -> Main.Model
playAll moves =
    List.foldl (\i m -> Main.update (Play i) m) Main.init moves


suite : Test
suite =
    describe "Tic-tac-toe logic"
        [ test "makeWinCons has 8 lines including both diagonals" <|
            \_ ->
                Main.makeWinCons
                    |> Expect.all
                        [ List.length >> Expect.equal 8
                        , List.member [ 0, 4, 8 ] >> Expect.equal True
                        , List.member [ 2, 4, 6 ] >> Expect.equal True
                        ]
        , test "toggleTurn alternates between X and O" <|
            \_ ->
                ( Main.toggleTurn X, Main.toggleTurn O )
                    |> Expect.equal ( O, X )
        , test "matchCell only fills empty cells" <|
            \_ ->
                ( Main.matchCell X Empty, Main.matchCell X O )
                    |> Expect.equal ( X, O )
        , test "getIsWinSlice detects three in a row of the same mark" <|
            \_ ->
                List.map Main.getIsWinSlice
                    [ [ X, X, X ], [ O, O, O ], [ X, O, X ], [ Empty, Empty, Empty ] ]
                    |> Expect.equal [ True, True, False, False ]
        , test "checkWin finds a winning top row" <|
            \_ ->
                Main.checkWin [ X, X, X, O, O, Empty, Empty, Empty, Empty ] Main.makeWinCons
                    |> List.any identity
                    |> Expect.equal True
        , test "first move places X and passes the turn to O" <|
            \_ ->
                playAll [ 4 ]
                    |> (\m -> ( Main.getCellStateAt 4 m.board, m.turn ))
                    |> Expect.equal ( X, O )
        , test "playing an occupied cell does not change board or turn" <|
            \_ ->
                playAll [ 0, 0 ]
                    |> (\m -> ( m.board, m.turn ))
                    |> Expect.equal ( X :: List.repeat 8 Empty, O )
        , test "X wins the top row and the game ends" <|
            \_ ->
                playAll [ 0, 3, 1, 4, 2 ]
                    |> (\m -> ( m.gameState, m.turn, m.winSlice ))
                    |> Expect.equal ( GameOver, X, [ 0, 1, 2 ] )
        , test "a full board with no line (draw) stays Playing" <|
            \_ ->
                playAll [ 0, 1, 2, 4, 3, 5, 7, 6, 8 ]
                    |> (\m -> ( m.gameState, List.member Empty m.board ))
                    |> Expect.equal ( Playing, False )
        , test "Restart returns to the initial model" <|
            \_ ->
                Main.update Restart (playAll [ 0, 3, 1, 4, 2 ])
                    |> Expect.equal Main.init
        ]
