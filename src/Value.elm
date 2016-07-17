module Value
    exposing
        ( Value
        , toString
        , makeInt
        , makeFloat
        , makeBool
        , makeString
        , makeDate
        , S
        , tests
        )

{-| This module wraps all the possible types a table cell can have


@docs Value, toString, makeInt, makeFloat, makeBool, makeString,makeDate, tests, S

-}

import ElmTest exposing (..)
import Html exposing (..)
import Date exposing (..)


type I
    = Int


type F
    = Float


type B
    = Bool


{-| String type
-}
type S
    = String


type D
    = Date


{-| This type holds the possible types for a table cell
-}
type Val
    = I Int
    | F Float
    | B Bool
    | S String
    | D Int


{-| This type wraps the Val type
-}
type Value a
    = W Val


{-| Create Integer
-}
makeInt : Int -> Value I
makeInt v =
    W (I v)


{-| Create Float
-}
makeFloat : Float -> Value F
makeFloat v =
    W (F v)


{-| Create Boolean
-}
makeBool : Bool -> Value B
makeBool v =
    W (B v)


{-| Create String
-}
makeString : String -> Value S
makeString v =
    W (S v)


{-| Create Date
-}
makeDate : Int -> Value D
makeDate v =
    W (D v)


{-| String representation of a value
-}
toString : Value a -> String
toString value =
    case value of
        W (I int) ->
            Basics.toString int

        W (F float) ->
            Basics.toString float

        W (B bool) ->
            Basics.toString bool

        W (S string) ->
            string

        W (D timestamp) ->
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
                    if (Date.hour date) < 10 then
                        "0" ++ (Date.hour date |> Basics.toString)
                    else
                        Date.hour date |> Basics.toString

                mins =
                    if (Date.minute date) < 10 then
                        "0" ++ (Date.minute date |> Basics.toString)
                    else
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
        [ test "Integer" <| assertEqual (toString (makeInt 23)) "23"
        , test "Float" <| assertEqual (toString (makeFloat 23.33)) "23.33"
        , test "Boolean" <| assertEqual (toString (makeBool False)) "False"
        , test "Date" <| assertEqual (toString (makeDate 1468688316)) "16 July 2016, 19:58"
        , test "String" <| assertEqual (toString (makeString "Test string")) "Test string"
        ]


{-| This is just to visually check this module
-}
main : Html a
main =
    div []
        [ div [] [ text (toString (makeInt 2)) ]
        , div [] [ text (toString (makeFloat 2.3)) ]
        , div [] [ text (toString (makeDate 1468688316)) ]
        , div [] [ text (toString (makeBool False)) ]
        , div [] [ text (toString (makeString "Test")) ]
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
