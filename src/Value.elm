module Value exposing (tests, Value)

{-| This module wraps all the possible types a table cell can have


@docs tests, Value

-}

import ElmTest exposing (..)
import Html exposing (..)
import Date exposing (..)


{-| This type holds the possible types for a table cell
-}
type Value
    = I Int
    | F Float
    | B Bool
    | S String
    | D Int


toString : Value -> String
toString value =
    case value of
        I int ->
            Basics.toString int

        F float ->
            Basics.toString float

        B bool ->
            Basics.toString bool

        S string ->
            string

        D timestamp ->
            let
                date =
                    getDateFromInt timestamp

                day =
                    Date.day date |> Basics.toString

                month =
                    getMonth date

                year =
                    Date.year date |> Basics.toString

                hours =
                    Date.hour date |> Basics.toString

                mins =
                    Date.minute date |> Basics.toString
            in
                day ++ " " ++ month ++ " " ++ year ++ ", " ++ hours ++ ":" ++ mins


getDateFromInt : Int -> Date
getDateFromInt timestamp =
    (1000 * timestamp) |> toFloat |> Date.fromTime


{-| The function with all the necessary tests
-}
tests : Test
tests =
    suite "Value package Test suite"
        [ test "Addition" <| assertEqual (3 + 5) 8
        ]


{-| This is just to visually check this module
-}
main : Html a
main =
    div []
        [ div [] [ text (toString (I 2)) ]
        , div [] [ text (toString (F 2.3)) ]
        , div [] [ text (toString (D 1468688316)) ]
        , div [] [ text (toString (B False)) ]
        , div [] [ text (toString (S "Test")) ]
        ]


{-| Helper method to convert month type into its string representation
-}
getMonth : Date -> String
getMonth date =
    let
        m =
            Date.month date
    in
        case m of
            Jan ->
                "January"

            Feb ->
                "February"

            Mar ->
                "March"

            Apr ->
                "April"

            May ->
                "May"

            Jun ->
                "June"

            Jul ->
                "July"

            Aug ->
                "August"

            Sep ->
                "September"

            Oct ->
                "October"

            Nov ->
                "November"

            Dec ->
                "December"
