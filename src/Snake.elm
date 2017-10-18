module Snake exposing (snakeApp)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Snake.Model exposing (..)
import Keyboard
import Time exposing (Time, millisecond)
import Random
import List.Extra exposing ((!!))
import List.Min2Elems as List2
import Svg
import Svg.Attributes as SvgAttrs


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
        , snake = List2.withAtLeastTwoElements ( 2, 2 ) ( 3, 2 ) [ ( 4, 2 ) ]
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
    if List2.head activeGame.snake == coord then
        SnakeTile <|
            { snakePart = SnakeHead
            , direction = activeGame.direction
            }
    else if List2.last activeGame.snake == coord then
        SnakeTile <|
            { snakePart = SnakeTail
            , direction = deriveTailDirection activeGame.snake
            }
    else if List2.member coord activeGame.snake then
        SnakeTile <| deriveSnakeBody coord activeGame
    else if Just coord == activeGame.food then
        FoodTile
    else
        FreeTile


deriveTailDirection : Snake -> Direction
deriveTailDirection snake =
    let
        snakeLength =
            List2.length snake

        finalTwo =
            List2.drop (snakeLength - 2) snake

        direction =
            directionBetween finalTwo.neck finalTwo.head
    in
        direction


deriveSnakeBody : Coord -> ActiveGame -> DirectedSnakePartView
deriveSnakeBody coord activeGame =
    -- Prerequisite: Assumes we already know the coord is part of the snake,
    -- and that we know the snake is at least length 3.
    let
        snake =
            activeGame.snake

        maybeIndex =
            List2.elemIndex coord snake
    in
        case maybeIndex of
            -- Should never happen, we've already determined the coord is
            -- part of the snake.
            -- TODO: Redesign data types to avoid so many "should never
            -- happen"s.
            Nothing ->
                { snakePart = SnakeBody Forward
                , direction = activeGame.direction
                }

            Just index ->
                let
                    bodyCoords =
                        snake
                            |> List2.drop (index - 1)
                            |> List2.take 3

                    bh =
                        bodyCoords.head

                    bb =
                        bodyCoords.neck

                    btMaybe =
                        List.head bodyCoords.rest

                    newDir =
                        directionBetween bb bh
                in
                    case btMaybe of
                        Just bt ->
                            let
                                prevDir =
                                    directionBetween bt bb

                                turnDir =
                                    turningDirectionOf prevDir newDir
                            in
                                { snakePart = SnakeBody turnDir
                                , direction = newDir
                                }

                        Nothing ->
                            -- Should never happen, we know the snake is at
                            -- least 3 coords long at this point
                            { snakePart = SnakeBody Forward
                            , direction = newDir
                            }


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
    let
        content =
            case tile of
                SnakeTile snakePart ->
                    [ Svg.svg
                        [ SvgAttrs.viewBox "-50 -50 100 100" ]
                        [ Svg.g [ SvgAttrs.transform "scale(1,-1)" ]
                            [ snakePartSvg snakePart ]
                        ]
                    ]

                _ ->
                    []
    in
        div
            [ class <| "tile " ++ (tileClass tile) ]
            content


snakePartSvg : DirectedSnakePartView -> Svg.Svg a
snakePartSvg { snakePart, direction } =
    let
        template : String -> Svg.Svg a
        template polygonPoints =
            let
                degreesRotation =
                    degreesRotationBetween direction Right
            in
                Svg.g [ SvgAttrs.transform <| "rotate(" ++ (toString degreesRotation) ++ ")" ]
                    [ Svg.polygon
                        [ SvgAttrs.points polygonPoints
                        , SvgAttrs.style "fill:lime;stroke:purple;stroke-width:1"
                        ]
                        []
                    ]
    in
        case snakePart of
            SnakeHead ->
                template headSvgPoints

            SnakeTail ->
                template tailSvgPoints

            SnakeBody turnDir ->
                case turnDir of
                    Forward ->
                        template bodyForwardSvgPoints

                    LeftTurn ->
                        template bodyLeftTurnSvgPoints

                    RightTurn ->
                        template bodyRightTurnSvgPoints


headSvgPoints : String
headSvgPoints =
    "-50,25 -35,25 -25,35 25,35 35,25 35,-25 25,-35 -25,-35 -35,-25 -50,-25"


bodyForwardSvgPoints : String
bodyForwardSvgPoints =
    "-50,25 50,25 50,-25 -50,-25"


bodyLeftTurnSvgPoints : String
bodyLeftTurnSvgPoints =
    --    "-50,25 -25,25 -25,50 25,50 25,-25 -50,-25"
    "-25,50 -25,-25 50,-25 50,25 25,25 25,50"


bodyRightTurnSvgPoints : String
bodyRightTurnSvgPoints =
    "-25,-50 -25,25 50,25 50,-25 25,-25 25,-50"


tailSvgPoints : String
tailSvgPoints =
    "50,25 0,25 -25,0 0,-25 50,-25"


tileClass : Tile -> String
tileClass tile =
    case tile of
        SnakeTile { snakePart, direction } ->
            case snakePart of
                SnakeHead ->
                    "snake head"

                SnakeBody turnDir ->
                    "snake"

                SnakeTail ->
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
    directionBetween snake.neck snake.head


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


degreesRotationBetween : Direction -> Direction -> Int
degreesRotationBetween fromDir toDir =
    if toDir == clockwiseDirectionOf fromDir then
        90
    else if toDir == oppositeDirection fromDir then
        180
    else if toDir == counterClockwiseDirectionOf fromDir then
        -90
    else
        0


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



-- Copied from https://gist.github.com/fredcy/5746b0af5ddb3f23f27470f41c883f86


cartesian : List a -> List b -> List ( a, b )
cartesian xs ys =
    List.concatMap
        (\x -> List.map (\y -> ( x, y )) ys)
        xs


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
