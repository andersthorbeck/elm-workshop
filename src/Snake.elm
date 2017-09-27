module Snake exposing (snakeApp)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Snake.Model exposing (..)


snakeApp =
    Html.beginnerProgram
        { model = testModel
        , view = view
        , update = update
        }


testModel : Model
testModel =
    Playing
        { gridDims = ( 6, 5 )
        , snake = [ ( 2, 2 ), ( 3, 2 ), ( 4, 2 ) ]
        , direction = Left
        , food = ( 1, 3 )
        }


view : Model -> Html Msg
view model =
    let
        gameView =
            case model of
                Playing activeGame ->
                    viewPlaying activeGame

                GameOver ->
                    viewGameOver
    in
        div []
            [ h1 []
                [ text "Snake" ]
            , div []
                gameView
            ]


viewGameOver : List (Html a)
viewGameOver =
    [ text "Snek ded" ]


viewPlaying : ActiveGame -> List (Html Msg)
viewPlaying activeGame =
    [ div []
        [ viewGrid (toGrid activeGame) ]
    , div []
        -- TODO: Next step: Listen to keydowns to generate these messages. Look up subscriptions.
        [ button [ onClick (ChangeDirection Left) ] [ text "<" ]
        , button [ onClick (ChangeDirection Up) ] [ text "^" ]
        , button [ onClick (ChangeDirection Down) ] [ text "v" ]
        , button [ onClick (ChangeDirection Right) ] [ text ">" ]
        ]
    , div []
        [ button [ onClick Tick ] [ text "tick" ] ]
    ]


toGrid : ActiveGame -> Grid
toGrid model =
    let
        ( _, numRows ) =
            model.gridDims

        -- Rows are numbered from the bottom, but rendered from the top.
        rows =
            List.reverse (List.range 0 (numRows - 1))
    in
        List.map (\r -> toRow model r) rows


toRow : ActiveGame -> Int -> Row
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


toTile : ActiveGame -> Coord -> Tile
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
    case model of
        GameOver ->
            GameOver

        Playing activeGame ->
            case msg of
                ChangeDirection dir ->
                    Playing (changeDirection dir activeGame)

                Tick ->
                    Playing (tick activeGame)


changeDirection : Direction -> ActiveGame -> ActiveGame
changeDirection dir model =
    if List.member dir (legalDirectionChanges model.snake) then
        { model | direction = dir }
    else
        model


legalDirectionChanges : Snake -> List Direction
legalDirectionChanges snake =
    let
        -- We derive direction from last tick instead of using current direction
        -- directly from model, to avoid issues where you might change direction
        -- several times between consecutive ticks.
        currDir =
            deriveDirectionLastTick snake
    in
        List.filter (\d -> d /= oppositeDirection currDir) allDirections


deriveDirectionLastTick : Snake -> Direction
deriveDirectionLastTick snake =
    case snake of
        head :: neck :: _ ->
            let
                ( xDiff, yDiff ) =
                    subtract head neck
            in
                if (abs xDiff) >= (abs yDiff) then
                    if xDiff >= 0 then
                        Right
                    else
                        Left
                else if yDiff >= 0 then
                    Up
                else
                    Down

        _ ->
            -- This catch-all should never be matched, as the snake should never
            -- be less than 2 tiles long.
            Right


subtract : Coord -> Coord -> Coord
subtract ( x1, y1 ) ( x2, y2 ) =
    ( x1 - x2, y1 - y2 )


allDirections : List Direction
allDirections =
    [ Up, Down, Left, Right ]


oppositeDirection : Direction -> Direction
oppositeDirection dir =
    case dir of
        Up ->
            Down

        Down ->
            Up

        Left ->
            Right

        Right ->
            Left


tick : ActiveGame -> ActiveGame
tick model =
    let
        -- The default should never be used, as the snake should never be 0 length.
        originalHead =
            Maybe.withDefault ( 0, 0 ) (List.head model.snake)

        newHead =
            nextHead model.direction originalHead

        newTail =
            dropLast model.snake
    in
        -- TODO: Handle collisions
        if newHead == model.food then
            let
                intermediateModel =
                    { model | snake = newHead :: model.snake }
            in
                { intermediateModel
                    | food = generateRandomFood intermediateModel
                }
        else
            { model | snake = newHead :: newTail }


nextHead : Direction -> Coord -> Coord
nextHead dir ( x, y ) =
    case dir of
        Up ->
            ( x, y + 1 )

        Down ->
            ( x, y - 1 )

        Left ->
            ( x - 1, y )

        Right ->
            ( x + 1, y )


dropLast : List a -> List a
dropLast l =
    case l of
        [] ->
            -- TODO: This should strictly lead to an error, but I don't know how.
            []

        [ x ] ->
            []

        x :: xs ->
            x :: dropLast xs


generateRandomFood : ActiveGame -> Food
generateRandomFood model =
    -- TODO: Make this actually random
    let
        eligibleFoodCoords =
            List.filter
                (\c -> not (List.member c model.snake))
                (enumerateAllGridCoords model.gridDims)
    in
        Maybe.withDefault ( 0, 0 ) (List.head eligibleFoodCoords)


enumerateAllGridCoords : GridDims -> List Coord
enumerateAllGridCoords ( width, height ) =
    let
        xs =
            List.range 0 (width - 1)

        ys =
            List.range 0 (height - 1)
    in
        cartesian xs ys



-- Copied from https://gist.github.com/fredcy/5746b0af5ddb3f23f27470f41c883f86


cartesian : List a -> List b -> List ( a, b )
cartesian xs ys =
    List.concatMap
        (\x -> List.map (\y -> ( x, y )) ys)
        xs
