module Snake.Model exposing (..)


type alias Test =
    String



-- Model types


type alias Model =
    { gridDims : GridDims
    , snake : Snake
    , direction : Direction
    , food : Food
    }



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
    List Coord


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



-- Helper types


type alias Grid =
    List Row


type alias Row =
    List Tile


type Tile
    = SnakeTile
    | FoodTile
    | FreeTile


type alias SnakeLength =
    Int
