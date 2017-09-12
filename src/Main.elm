module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


main : Html String
main =
    viewCards cards


type CardState
    = Open
    | Closed
    | Matched


type alias Card =
    { id : String, state : CardState }


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


viewCards : List Card -> Html a
viewCards cards =
    div []
        (List.map viewCard cards)
