module Snake exposing (snakeApp)

import Html exposing (..)
import Html.Attributes exposing (..)
import Snake.Model exposing (..)


snakeApp : Html a
snakeApp =
    view testGrid


testGrid : Grid
testGrid =
    [ [ SnakeTile, FreeTile ], [ FreeTile, FoodTile ] ]


view : Grid -> Html a
view grid =
    div []
        [ h1 []
            [ text "Snake" ]
        , div []
            [ viewGrid grid ]
        ]


viewGrid : Grid -> Html a
viewGrid grid =
    div
        [ class "grid" ]
        (List.map viewRow grid)


viewRow : Row -> Html a
viewRow row =
    div
        [ class "row" ]
        (List.map viewTile row)


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
            [ class "tile"
            , style
                [ ( "background-color", color )
                ]
            ]
            []
