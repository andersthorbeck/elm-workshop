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
    | SnakeTile SnakePartView
    | FoodTile


type SnakePartView
    = SnakeHead Direction
    | SnakeBody TurningDirection Direction
    | SnakeTail Direction


type TurningDirection
    = Forward
    | LeftTurn
    | RightTurn
