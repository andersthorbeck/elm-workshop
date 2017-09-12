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


type alias Card =
    { id : String, state : CardState }


firstCard : Card
firstCard =
    { id = "1", state = Open }


viewCard : Card -> Html a
viewCard card =
    div []
        [ img [ src ("/static/cats/" ++ card.id ++ ".jpg") ]
            []
        ]
