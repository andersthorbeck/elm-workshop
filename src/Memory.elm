module Memory exposing (memoryApp)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import MemoryModel exposing (..)
import DeckGenerator exposing (..)
import Random


memoryApp =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


init : ( Model, Cmd Msg )
init =
    ( Choosing (DeckGenerator.static), Cmd.none )


view : Model -> Html Msg
view model =
    case model of
        Choosing deck ->
            viewCards deck

        Matching deck card ->
            viewCards deck

        GameOver ->
            viewGameOver


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
        div [ style [ ( "display", "inline-block" ) ] ]
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


viewGameOver : Html Msg
viewGameOver =
    div []
        [ p [] [ text "Game over, well done!" ]
        , button [ onClick Restart ] [ text "Restart!" ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CardClick card ->
            ( updateOnCardClick card model, Cmd.none )

        Restart ->
            ( model, Random.generate OnGenerated (DeckGenerator.random) )

        OnGenerated deck ->
            ( Choosing deck, Cmd.none )


updateOnCardClick : Card -> GameState -> GameState
updateOnCardClick card state =
    case state of
        Choosing deck ->
            Matching
                (openGivenCardInDeck card (closeAllUnmatched deck))
                card

        Matching deck prevSelectedCard ->
            if (doCardsMatch prevSelectedCard card) then
                let
                    updatedDeck =
                        setCardsMatched prevSelectedCard card deck
                in
                    if (isGameOver updatedDeck) then
                        GameOver
                    else
                        Choosing updatedDeck
            else
                Choosing (openGivenCardInDeck card deck)

        GameOver ->
            state


closeAllUnmatched : Deck -> Deck
closeAllUnmatched =
    List.map closeIfUnmatched


closeIfUnmatched : Card -> Card
closeIfUnmatched =
    callIf (\c -> c.state /= Matched) (setCard Closed)


setCard : CardState -> Card -> Card
setCard newState card =
    { card | state = newState }


callIf : (Card -> Bool) -> (Card -> Card) -> (Card -> Card)
callIf condition function =
    (\c ->
        if (condition c) then
            function c
        else
            c
    )


openGivenCardInDeck : Card -> Deck -> Deck
openGivenCardInDeck card =
    List.map (openGivenCard card)


openGivenCard : Card -> (Card -> Card)
openGivenCard card =
    callIf (\c -> c == card) (setCard Open)


doCardsMatch : Card -> Card -> Bool
doCardsMatch c1 c2 =
    c1.id == c2.id && c1.group /= c2.group


setCardsMatched : Card -> Card -> Deck -> Deck
setCardsMatched c1 c2 deck =
    List.map
        (callIf
            (\c -> List.any (doCardsMatch c) [ c1, c2 ])
            (setCard Matched)
        )
        deck


isGameOver : Deck -> Bool
isGameOver =
    areAllCardsMatched


areAllCardsMatched : Deck -> Bool
areAllCardsMatched =
    List.all (\c -> c.state == Matched)
