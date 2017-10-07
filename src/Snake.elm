module Snake exposing (snakeApp)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Snake.Model exposing (..)
import Keyboard
import Time exposing (Time, millisecond)
import Random


snakeApp =
    Html.program
        { init = ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


initialModel : Model
initialModel =
    Playing
        { gridDims = ( 6, 5 )
        , snake = [ ( 2, 2 ), ( 3, 2 ), ( 4, 2 ) ]
        , direction = Left
        , food = Just ( 1, 3 )
        }


view : Model -> Html Msg
view model =
    let
        gameView =
            case model of
                Playing activeGame ->
                    viewPlaying activeGame

                GameOver health ->
                    viewGameOver health
    in
        div []
            [ h1 []
                [ text "Snake" ]
            , div []
                gameView
            ]


viewGameOver : Health -> List (Html Msg)
viewGameOver health =
    let
        gameOverText =
            case health of
                Dead ->
                    "Snek ded"

                Alive ->
                    "You win!"
    in
        [ p [] [ text gameOverText ]
        , button [ onClick RestartGame ] [ text "Restart" ]
        ]


viewPlaying : ActiveGame -> List (Html Msg)
viewPlaying activeGame =
    [ div []
        [ activeGame |> toGrid |> viewGrid ]
    ]


toGrid : ActiveGame -> Grid
toGrid activeGame =
    let
        ( _, numRows ) =
            activeGame.gridDims

        -- Rows are numbered from the bottom, but rendered from the top.
        rows =
            List.reverse <| List.range 0 <| numRows - 1
    in
        List.map (toRow activeGame) rows


toRow : ActiveGame -> Int -> Row
toRow activeGame rowNum =
    let
        -- Remember, the row is the _vertical_ component, i.e. the y, not the x.
        y =
            rowNum

        ( numColumns, _ ) =
            activeGame.gridDims

        columns =
            List.range 0 <| numColumns - 1
    in
        List.map
            (\x -> toTile activeGame ( x, y ))
            columns


toTile : ActiveGame -> Coord -> Tile
toTile activeGame coord =
    if List.head activeGame.snake == Just coord then
        SnakeHeadTile activeGame.direction
    else if last activeGame.snake == Just coord then
        SnakeTailTile <| deriveTailDirection activeGame.snake
    else if List.member coord activeGame.snake then
        deriveSnakeBodyTile coord activeGame
    else if Just coord == activeGame.food then
        FoodTile
    else
        FreeTile


deriveTailDirection : Snake -> Direction
deriveTailDirection snake =
    let
        snakeLength =
            List.length snake

        finalTwo =
            List.drop (snakeLength - 2) snake

        direction =
            case finalTwo of
                almostTail :: tail :: [] ->
                    directionBetween tail almostTail

                _ ->
                    -- Should never happen
                    Left
    in
        direction


deriveSnakeBodyTile : Coord -> ActiveGame -> Tile
deriveSnakeBodyTile coord activeGame =
    -- Prerequisite: Assumes we already know the coord is part of the snake,
    -- and that we know the snake is at least length 3.
    let
        snake =
            activeGame.snake

        maybeIndex =
            elemIndex coord snake
    in
        case maybeIndex of
            -- Should never happen, we've already determined the coord is
            -- part of the snake.
            -- TODO: Redesign data types to avoid so many "should never
            -- happen"s.
            Nothing ->
                SnakeBodyTile Forward activeGame.direction

            Just index ->
                let
                    bodyCoords =
                        snake
                            |> List.drop (index - 1)
                            |> List.take 3
                in
                    case bodyCoords of
                        [ bh, bb, bt ] ->
                            let
                                prevDir =
                                    directionBetween bt bb

                                newDir =
                                    directionBetween bb bh

                                turnDir =
                                    turningDirectionOf prevDir newDir
                            in
                                SnakeBodyTile turnDir newDir

                        _ ->
                            -- Should never happen, we know the snake is at
                            -- least 3 coords long at this point
                            SnakeBodyTile Forward activeGame.direction


turningDirectionOf : Direction -> Direction -> TurningDirection
turningDirectionOf prevDir newDir =
    if newDir == clockwiseDirectionOf prevDir then
        RightTurn
    else if newDir == counterClockwiseDirectionOf prevDir then
        LeftTurn
    else
        -- newDir should never be the opposite direction of prevDir,
        -- default to Forward
        Forward


viewGrid : Grid -> Html a
viewGrid grid =
    -- TODO: More detailed snake-view
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
        [ class <| "tile " ++ (tileClass tile) ]
        []


tileClass : Tile -> String
tileClass tile =
    case tile of
        SnakeHeadTile dir ->
            "snake head"

        SnakeBodyTile turnDir dir ->
            "snake"

        SnakeTailTile dir ->
            "snake tail"

        FoodTile ->
            "food"

        FreeTile ->
            "unoccupied"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        GameOver health ->
            case msg of
                RestartGame ->
                    ( initialModel, Cmd.none )

                _ ->
                    ( GameOver health, Cmd.none )

        Playing activeGame ->
            case msg of
                -- TODO: queue direction changes?
                ChangeDirection dir ->
                    ( Playing <| changeDirection dir activeGame, Cmd.none )

                Tick ->
                    tick activeGame

                NewFood index ->
                    let
                        newFood =
                            -- The default should never be used, as the index
                            -- should always be generated in range
                            (eligibleFoodCoords activeGame)
                                !! index
                                |> Maybe.withDefault ( 0, 0 )
                    in
                        ( Playing { activeGame | food = Just newFood }
                        , Cmd.none
                        )

                _ ->
                    ( Playing activeGame, Cmd.none )


changeDirection : Direction -> ActiveGame -> ActiveGame
changeDirection dir activeGame =
    if List.member dir <| legalDirectionChanges activeGame.snake then
        { activeGame | direction = dir }
    else
        activeGame


legalDirectionChanges : Snake -> List Direction
legalDirectionChanges snake =
    let
        -- We derive direction from last tick instead of using current direction
        -- directly from activeGame, to avoid issues where you might change direction
        -- several times between consecutive ticks.
        currDir =
            deriveDirectionLastTick snake
    in
        List.filter (\d -> d /= oppositeDirection currDir) allDirections


deriveDirectionLastTick : Snake -> Direction
deriveDirectionLastTick snake =
    case snake of
        head :: neck :: _ ->
            directionBetween neck head

        _ ->
            -- This catch-all should never be matched, as the snake should never
            -- be less than 2 tiles long.
            Right


directionBetween : Coord -> Coord -> Direction
directionBetween from to =
    let
        ( xDiff, yDiff ) =
            subtract to from
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


clockwiseDirectionOf : Direction -> Direction
clockwiseDirectionOf dir =
    case dir of
        Up ->
            Right

        Right ->
            Down

        Down ->
            Left

        Left ->
            Up


counterClockwiseDirectionOf : Direction -> Direction
counterClockwiseDirectionOf dir =
    case dir of
        Up ->
            Left

        Left ->
            Down

        Down ->
            Right

        Right ->
            Up


tick : ActiveGame -> ( Model, Cmd Msg )
tick activeGame =
    let
        ( updatedGame, cmd ) =
            uncheckedTick activeGame
    in
        if doesSnakeCoverEntireGrid updatedGame then
            ( GameOver Alive, cmd )
        else if isGameOver updatedGame then
            ( GameOver Dead, cmd )
        else
            ( Playing updatedGame, cmd )


uncheckedTick : ActiveGame -> ( ActiveGame, Cmd Msg )
uncheckedTick activeGame =
    let
        -- The default should never be used, as the snake should never be 0 length.
        originalHead =
            List.head activeGame.snake |> Maybe.withDefault ( 0, 0 )

        newHead =
            nextHead activeGame.direction originalHead

        newTail =
            dropLast activeGame.snake
    in
        if Just newHead == activeGame.food then
            let
                intermediateGame =
                    { activeGame
                        | snake = newHead :: activeGame.snake
                        , food = Nothing
                    }

                numEligibleFoodCoords =
                    List.length <| eligibleFoodCoords intermediateGame
            in
                ( intermediateGame
                , Random.generate NewFood
                    (Random.int 0 (numEligibleFoodCoords - 1))
                )
        else
            ( { activeGame | snake = newHead :: newTail }, Cmd.none )


doesSnakeCoverEntireGrid : ActiveGame -> Bool
doesSnakeCoverEntireGrid activeGame =
    let
        ( width, height ) =
            activeGame.gridDims
    in
        (List.length activeGame.snake) == width * height


isGameOver : ActiveGame -> Bool
isGameOver activeGame =
    hasSnakeEatenSelf activeGame.snake || isSnakeOutsideGrid activeGame


hasSnakeEatenSelf : Snake -> Bool
hasSnakeEatenSelf snake =
    case snake of
        [] ->
            -- Will never occur
            False

        head :: tail ->
            List.member head tail


isSnakeOutsideGrid : ActiveGame -> Bool
isSnakeOutsideGrid activeGame =
    case activeGame.snake of
        [] ->
            -- Will never occur
            False

        head :: _ ->
            not <| isCoordWithinGrid activeGame.gridDims head


isCoordWithinGrid : GridDims -> Coord -> Bool
isCoordWithinGrid ( width, height ) ( x, y ) =
    0 <= x && x < width && 0 <= y && y < height


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


eligibleFoodCoords : ActiveGame -> List Coord
eligibleFoodCoords activeGame =
    List.filter
        (\c -> not <| List.member c activeGame.snake)
    <|
        enumerateAllGridCoords activeGame.gridDims


enumerateAllGridCoords : GridDims -> List Coord
enumerateAllGridCoords ( width, height ) =
    let
        xs =
            List.range 0 <| width - 1

        ys =
            List.range 0 <| height - 1
    in
        cartesian xs ys



-- Copied from https://gist.github.com/fredcy/5746b0af5ddb3f23f27470f41c883f86


cartesian : List a -> List b -> List ( a, b )
cartesian xs ys =
    List.concatMap
        (\x -> List.map (\y -> ( x, y )) ys)
        xs


infixl 9 !!
(!!) : List a -> Int -> Maybe a
(!!) xs n =
    List.head <| List.drop n xs


last : List a -> Maybe a
last xs =
    xs |> List.reverse |> List.head


elemIndex : a -> List a -> Maybe Int
elemIndex x xs =
    let
        elemIndexHelper x xs n =
            case xs of
                [] ->
                    Nothing

                y :: ys ->
                    if x == y then
                        Just n
                    else
                        elemIndexHelper x ys (n + 1)
    in
        elemIndexHelper x xs 0


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        GameOver _ ->
            Sub.none

        Playing _ ->
            Sub.batch
                [ Keyboard.downs keyCodeToMsg
                , Time.every (500 * millisecond) (\_ -> Tick)
                ]


keyCodeToMsg : Keyboard.KeyCode -> Msg
keyCodeToMsg keyCode =
    case keyCodeToDirection keyCode of
        Just direction ->
            ChangeDirection direction

        Nothing ->
            NoOp


keyCodeToDirection : Keyboard.KeyCode -> Maybe Direction
keyCodeToDirection keyCode =
    case keyCode of
        37 ->
            Just Left

        38 ->
            Just Up

        39 ->
            Just Right

        40 ->
            Just Down

        _ ->
            Nothing
