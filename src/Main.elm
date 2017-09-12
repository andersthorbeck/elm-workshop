module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias Model =
    { cards : Deck }


type alias Deck =
    List Card


type alias Card =
    { id : String, state : CardState, group : CardGroup }


type CardState
    = Open
    | Closed
    | Matched


type CardGroup
    = A
    | B


type Msg
    = CardClick Card


init : Model
init =
    { cards = cards }


cards : Deck
cards =
    [ { id = "1", state = Open, group = A }
    , { id = "2", state = Closed, group = A }
    , { id = "3", state = Matched, group = A }
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


viewCards : Deck -> Html Msg
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
    updateOnCardClick card model


updateOnCardClick : Card -> Model -> Model
updateOnCardClick card model =
    { model
        | cards =
            List.map
                (openGivenCard card)
                model.cards
    }


openGivenCard : Card -> (Card -> Card)
openGivenCard card =
    callIf (\c -> c == card) (setCard Open)


callIf : (Card -> Bool) -> (Card -> Card) -> (Card -> Card)
callIf condition function =
    (\c ->
        if (condition c) then
            function c
        else
            c
    )


setCard : CardState -> Card -> Card
setCard newState card =
    { card | state = newState }
