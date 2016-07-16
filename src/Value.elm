module Value exposing (tests)

{-| This module wraps all the possible types a table cell can have


@docs tests

-}

import ElmTest exposing (..)


{-| The function with all the necessary tests
-}
tests : Test
tests =
    suite "Value package Test suite"
        [ test "Addition" <| assertEqual (3 + 5) 8
        ]
