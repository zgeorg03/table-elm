module Main exposing (..)

import ElmTest exposing (..)
import Value exposing (tests)


main : Program Never
main =
    runSuite tests
