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
    let
        imgPath imgName =
            "/static/cats/" ++ imgName

        ( srcUrl, className ) =
            case card.state of
                Closed ->
                    ( imgPath "closed.png", "closed" )

                Open ->
                    ( imgPath (card.id ++ ".jpg"), "open" )

                Matched ->
                    ( imgPath (card.id ++ ".jpg"), "matched" )
    in
        div []
            [ img
                [ src srcUrl, class className ]
                []
            ]


viewCards : List Card -> Html a
viewCards cards =
    div []
        (List.map viewCard cards)
