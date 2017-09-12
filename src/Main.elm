module Main exposing (..)

import Html exposing (..)


main : Html String
main =
    text (greet "Anders")


greet : String -> String
greet name =
    "Hello, " ++ name


thing : { id : String }
thing =
    { id = "1" }
