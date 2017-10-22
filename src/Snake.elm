module Snake exposing (snakeApp)

import Snake.Model exposing (..)
import Snake.View exposing (view)
import Snake.Update exposing (update)
import Snake.Subscriptions exposing (subscriptions)
import Html exposing (program)


snakeApp =
    Html.program
        { init = ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
