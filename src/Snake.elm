module Snake exposing (snakeApp)

import Html exposing (..)
import Html.Attributes exposing (..)
import Snake.Model exposing (..)


snakeApp =
    Html.beginnerProgram
        { model = testModel
        , view = view
        , update = update
        }


testModel : Model
testModel =
    { gridDims = ( 3, 3 )
    , snake = [ ( 1, 1 ), ( 2, 1 ) ]
    , direction = Left
    , food = ( 0, 2 )
    }


view : Model -> Html a
view model =
    div []
        [ h1 []
            [ text "Snake" ]
        , div []
            [ viewGrid (toGrid model) ]
        ]


toGrid : Model -> Grid
toGrid model =
    let
        ( _, numRows ) =
            model.gridDims

        -- Rows are numbered from the bottom, but rendered from the top.
        rows =
            List.reverse (List.range 0 (numRows - 1))
    in
        List.map (\r -> toRow model r) rows


toRow : Model -> Int -> Row
toRow model rowNum =
    let
        -- Remember, the row is the _vertical_ component, i.e. the y, not the x.
        y =
            rowNum

        ( numColumns, _ ) =
            model.gridDims

        columns =
            List.range 0 (numColumns - 1)
    in
        List.map
            (\x -> toTile model ( x, y ))
            columns


toTile : Model -> Coord -> Tile
toTile model coord =
    if List.member coord model.snake then
        SnakeTile
    else if coord == model.food then
        FoodTile
    else
        FreeTile


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
    div
        [ class ("tile " ++ (tileClass tile)) ]
        []


tileClass : Tile -> String
tileClass tile =
    case tile of
        SnakeTile ->
            "snake"

        FoodTile ->
            "food"

        FreeTile ->
            "unoccupied"


update : Msg -> Model -> Model
update msg model =
    -- WIP: no-op
    model
