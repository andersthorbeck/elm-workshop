module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


main : Html String
main =
    viewCard firstCard


type CardState
    = Open
    | Closed
    | Matched


type alias Card =
    { id : String, state : CardState }


firstCard : Card
firstCard =
    { id = "1", state = Open }


cards : List Card
cards =
    [ { id = "1", state = Open }
    , { id = "2", state = Closed }
    , { id = "3", state = Matched }
    ]


viewCard : Card -> Html a
viewCard card =
    div []
        [ img [ src ("/static/cats/" ++ card.id ++ ".jpg") ]
            []
        ]
