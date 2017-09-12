module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


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


main =
    Html.beginnerProgram
        { model = init
        , view = view
        , update = update
        }


view : Model -> Html Msg
view model =
    viewCards model.cards


viewCards : List Card -> Html Msg
viewCards cards =
    div []
        (List.map viewCard cards)


viewCard : Card -> Html Msg
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
                        , onClick (CardClick card)
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


update : Msg -> Model -> Model
update (CardClick card) model =
    { model
        | cards =
            List.map
                (\c ->
                    if (card == c) then
                        { c | state = Open }
                    else
                        c
                )
                model.cards
    }


setCard : CardState -> Card -> Card
setCard newState card =
    { card | state = newState }
