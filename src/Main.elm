module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


main : Html String
main =
    viewCard firstCard


greet : String -> String
greet name =
    "Hello, " ++ name


firstCard : { id : String }
firstCard =
    { id = "1" }


viewCard : { id : String } -> Html a
viewCard card =
    div []
        [ img [ src ("/static/cats/" ++ card.id ++ ".jpg") ]
            []
        ]
