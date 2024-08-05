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


type alias Model =
    { turn : Char
    , timewarp : List (List Char)
    , board : List Char
    }


init : Model
init =
    Model 'X' [ List.repeat 9 ' ' ] (List.repeat 9 ' ')



-- UPDATE


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            { model | turn = 'O' }

        Decrement ->
            { model | turn = 'X' }



-- VIEW


view : Model -> Html Msg
view model =
    div [ style "border" "1px solid #0a0305" ]
        [ div [] [ text "Hello" ]
        ]
