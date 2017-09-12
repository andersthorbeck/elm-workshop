module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


type alias Model =
    { cards : List Card }


type alias Card =
    { id : String, state : CardState }


type CardState
    = Open
    | Closed
    | Matched


type Msg
    = CardClick Card


init : Model
init =
    { cards = cards }


cards : List Card
cards =
    [ { id = "1", state = Open }
    , { id = "2", state = Closed }
    , { id = "3", state = Matched }
    ]


main : Html String
main =
    view init


view : Model -> Html a
view model =
    viewCards model.cards


viewCards : List Card -> Html a
viewCards cards =
    div []
        (List.map viewCard cards)


viewCard : Card -> Html a
viewCard card =
    let
        imgPath imgName =
            "/static/cats/" ++ imgName
    in
        div []
            [ img
                (case card.state of
                    Closed ->
                        [ src (imgPath "closed.png")
                        , class "closed"
                        ]

                    Open ->
                        [ src (imgPath (card.id ++ ".jpg"))
                        , class "open"
                        ]

                    Matched ->
                        [ src (imgPath (card.id ++ ".jpg"))
                        , class "matched"
                        ]
                )
                []
            ]
