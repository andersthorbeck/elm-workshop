module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


type alias Card =
    { id : String, state : CardState }


type CardState
    = Open
    | Closed
    | Matched


cards : List Card
cards =
    [ { id = "1", state = Open }
    , { id = "2", state = Closed }
    , { id = "3", state = Matched }
    ]


main : Html String
main =
    viewCards cards


viewCards : List Card -> Html a
viewCards cards =
    div []
        (List.map viewCard cards)


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
