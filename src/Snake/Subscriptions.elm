module Snake.Subscriptions exposing (subscriptions)

import Snake.Model exposing (..)
import Keyboard
import Time exposing (Time, millisecond)


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        GameOver _ ->
            Sub.none

        Playing activeGame ->
            if activeGame.paused then
                Sub.batch
                    [ Keyboard.downs keyCodeToMsg ]
            else
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
            if keyCode == spaceKey then
                TogglePause
            else
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


spaceKey : Keyboard.KeyCode
spaceKey =
    32
