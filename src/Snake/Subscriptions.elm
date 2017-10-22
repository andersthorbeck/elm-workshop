module Snake.Subscriptions exposing (subscriptions)

import Snake.Model exposing (..)
import Keyboard
import Time exposing (Time, millisecond)


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        GameOver _ ->
            Sub.none

        Playing _ ->
            Sub.batch
                [ Keyboard.downs keyCodeToMsg
                , Time.every (500 * millisecond) (\_ -> Tick)
                ]


keyCodeToMsg : Keyboard.KeyCode -> Msg
keyCodeToMsg keyCode =
    case keyCodeToDirection keyCode of
        Just direction ->
            ChangeDirection direction

        Nothing ->
            NoOp


keyCodeToDirection : Keyboard.KeyCode -> Maybe Direction
keyCodeToDirection keyCode =
    case keyCode of
        37 ->
            Just Left

        38 ->
            Just Up

        39 ->
            Just Right

        40 ->
            Just Down

        _ ->
            Nothing
