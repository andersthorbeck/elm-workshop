module Snake.View exposing (view)

import Snake.Model exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Min2Elems as List2
import Svg
import Svg.Attributes as SvgAttrs


-- View types


type alias Grid =
    List Row


type alias Row =
    List Tile


type Tile
    = FreeTile
    | SnakeTile DirectedSnakePartView
    | FoodTile


type alias DirectedSnakePartView =
    { snakePart : SnakePartView
    , direction : Direction
    }


type SnakePartView
    = SnakeHead
    | SnakeBody TurningDirection
    | SnakeTail


type TurningDirection
    = Forward
    | LeftTurn
    | RightTurn



-- View Functions


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
        degreesRotation =
            degreesRotationBetween Right direction

        polygonPoints =
            case snakePart of
                SnakeHead ->
                    headSvgPoints

                SnakeTail ->
                    tailSvgPoints

                SnakeBody turnDir ->
                    case turnDir of
                        Forward ->
                            bodyForwardSvgPoints

                        LeftTurn ->
                            bodyLeftTurnSvgPoints

                        RightTurn ->
                            bodyRightTurnSvgPoints
    in
        Svg.g
            [ SvgAttrs.transform <|
                "rotate("
                    ++ (toString degreesRotation)
                    ++ ")"
            ]
            [ Svg.polygon
                [ SvgAttrs.points polygonPoints
                , SvgAttrs.style "fill:lime;stroke:purple;stroke-width:1"
                ]
                []
            ]


degreesRotationBetween : Direction -> Direction -> Int
degreesRotationBetween fromDir toDir =
    -- The 90 degrees are opposite to what is expected because we flip the svg
    -- vertically
    if toDir == clockwiseDirectionOf fromDir then
        -90
    else if toDir == oppositeDirection fromDir then
        180
    else if toDir == counterClockwiseDirectionOf fromDir then
        90
    else
        0


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
