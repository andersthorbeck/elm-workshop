module Snake exposing (snakeApp)

import Html exposing (..)
import Html.Attributes exposing (..)
import Snake.Model exposing (..)


snakeApp : Html a
snakeApp =
    view SnakeTile


view : Tile -> Html a
view tile =
    div []
        [ h1 []
            [ text "Snake" ]
        , div []
            [ viewTile tile ]
        ]


viewTile : Tile -> Html a
viewTile tile =
    let
        color =
            case tile of
                SnakeTile ->
                    "yellow"

                FoodTile ->
                    "red"

                FreeTile ->
                    "gray"
    in
        div
            [ style
                [ ( "width", "10px" )
                , ( "height", "10px" )
                , ( "margin", "auto" )
                , ( "background-color", color )
                ]
            ]
            []
