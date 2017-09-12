module Main exposing (..)

import Html exposing (..)


main : Html String
main =
    text (greet "Anders")


greet name =
    "Hello, " ++ name
