module Model exposing (..)


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
