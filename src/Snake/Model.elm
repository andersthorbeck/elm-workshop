module Snake.Model exposing (..)

import Keyboard
import List.Min2Elems exposing (..)


type alias Test =
    String



-- Model types


type alias Model =
    GameState


type GameState
    = Playing ActiveGame
    | GameOver Health


type alias ActiveGame =
    { gridDims : GridDims
    , snake : Snake
    , direction : Direction
    , food : Maybe Food
    }


type Health
    = Alive
    | Dead



-- From origin (inclusive) to this coord (exclusive)


type alias GridDims =
    Coord


type alias Coord =
    ( Horizontal, Vertical )


type alias Horizontal =
    Int


type alias Vertical =
    Int



-- Head of Snake is first Coord of List


type alias Snake =
    ListMin2Elems Coord


type Direction
    = Up
    | Down
    | Left
    | Right


type alias Food =
    Coord



-- Msg types


type Msg
    = ChangeDirection Direction
    | Tick
    | NoOp
    | NewFood Int
    | RestartGame



-- Helper types


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



-- Model functions


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
