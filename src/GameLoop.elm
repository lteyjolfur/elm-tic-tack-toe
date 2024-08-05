module GameLoop exposing (..)

import Browser
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Browser.Events exposing (onAnimationFrameDelta)

-- MODEL

type alias Model =
    { x : Float }

initialModel : Model
initialModel =
    { x = 0 }

-- UPDATE

type Msg
    = Tick Float

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick deltaTime ->
            ( { model | x = model.x + deltaTime * 0.05 }, Cmd.none )

-- VIEW

view : Model -> Html Msg
view model =
    div
        [ style "position" "absolute"
        , style "left" (String.fromFloat model.x ++ "px")
        , style "top" "100px"
        , style "width" "100px"
        , style "height" "100px"
        , style "background-color" "blue"
        ]
        [ text "Moving Box" ]

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =

    onAnimationFrameDelta Tick

-- MAIN

main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
