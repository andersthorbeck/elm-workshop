module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


main : Html String
main =
    viewCard firstCard


greet : String -> String
greet name =
    "Hello, " ++ name


type CardState
    = Open
    | Closed
    | Matched


firstCard : { id : String, state : CardState }
firstCard =
    { id = "1", state = Open }


viewCard : { id : String, state : CardState } -> Html a
viewCard card =
    div []
        [ img [ src ("/static/cats/" ++ card.id ++ ".jpg") ]
            []
        ]
