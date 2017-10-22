module Snake.Update exposing (update)

import Snake.Model exposing (..)
import Random
import List.Extra exposing ((!!))
import List.Min2Elems as List2


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

                TogglePause ->
                    ( Playing { activeGame | paused = not activeGame.paused }
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
    directionBetween snake.neck snake.head


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
        originalHead =
            List2.head activeGame.snake

        newHead =
            nextHead activeGame.direction originalHead

        newTail =
            List2.dropLast activeGame.snake
    in
        if Just newHead == activeGame.food then
            let
                intermediateGame =
                    { activeGame
                        | snake = List2.cons newHead activeGame.snake
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
            ( { activeGame | snake = List2.cons newHead newTail }, Cmd.none )


doesSnakeCoverEntireGrid : ActiveGame -> Bool
doesSnakeCoverEntireGrid activeGame =
    let
        ( width, height ) =
            activeGame.gridDims
    in
        (List2.length activeGame.snake) == width * height


isGameOver : ActiveGame -> Bool
isGameOver activeGame =
    hasSnakeEatenSelf activeGame.snake || isSnakeOutsideGrid activeGame


hasSnakeEatenSelf : Snake -> Bool
hasSnakeEatenSelf snake =
    let
        tail =
            snake.neck :: snake.rest
    in
        List.member snake.head tail


isSnakeOutsideGrid : ActiveGame -> Bool
isSnakeOutsideGrid activeGame =
    not <| isCoordWithinGrid activeGame.gridDims activeGame.snake.head


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


eligibleFoodCoords : ActiveGame -> List Coord
eligibleFoodCoords activeGame =
    List.filter
        (\c -> not <| List2.member c activeGame.snake)
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


cartesian : List a -> List b -> List ( a, b )
cartesian xs ys =
    -- Copied from:
    -- https://gist.github.com/fredcy/5746b0af5ddb3f23f27470f41c883f86
    List.concatMap
        (\x -> List.map (\y -> ( x, y )) ys)
        xs
