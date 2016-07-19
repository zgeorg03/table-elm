module Value
    exposing
        ( Value(..)
        , ValueType(..)
        , getDefaultValueFromType
        , getDefaultValue
        , compare
        , toString
        , tests
        )

{-| This module wraps all the possible types a table cell can have


@docs Value,ValueType,getDefaultValueFromType, getDefaultValue, compare, toString, tests

-}

import ElmTest exposing (..)
import Html exposing (..)
import Date exposing (..)


{-| This type holds the possible types with the corresponding values for a table cell
-}
type Value
    = I Int
    | F Float
    | B Bool
    | S String
    | D Int


{-| This type holds the possible types for a table cell
-}
type ValueType
    = IntType
    | StringType
    | DateType
    | FloatType
    | BoolType


{-| Create Integer
-}
makeInt : Int -> Value
makeInt v =
    I v


{-| Create Integer type
-}
makeIntType : ValueType
makeIntType =
    IntType


{-| Create Float
-}
makeFloat : Float -> Value
makeFloat v =
    F v


{-| Create Boolean
-}
makeBool : Bool -> Value
makeBool v =
    B v


{-| Create String
-}
makeString : String -> Value
makeString v =
    S v


{-| Create Date
-}
makeDate : Int -> Value
makeDate v =
    D v


{-| Useful method to get the default value given it's data type
-}
getDefaultValueFromType : ValueType -> Value
getDefaultValueFromType valueType =
    case valueType of
        IntType ->
            I 0

        StringType ->
            S ""

        FloatType ->
            F 0.0

        DateType ->
            D 0

        BoolType ->
            B True


{-| Useful method to get the default value given it's value
-}
getDefaultValue : Value -> Value
getDefaultValue value =
    case value of
        I _ ->
            I 0

        S _ ->
            S ""

        F _ ->
            F 0.0

        D _ ->
            D 0

        B _ ->
            B True


{-| String representation of a value
-}
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


{-| Comparator for Value
-}
compare : Value -> Value -> Order
compare a b =
    case a of
        I v1 ->
            case b of
                I v2 ->
                    Basics.compare v1 v2

                _ ->
                    LT

        F v1 ->
            case b of
                F v2 ->
                    Basics.compare v1 v2

                _ ->
                    LT

        S v1 ->
            case b of
                S v2 ->
                    Basics.compare v1 v2

                _ ->
                    LT

        B v1 ->
            case b of
                B v2 ->
                    case ( v1, v2 ) of
                        ( True, False ) ->
                            GT

                        ( False, True ) ->
                            LT

                        _ ->
                            EQ

                _ ->
                    LT

        D v1 ->
            case b of
                D v2 ->
                    Basics.compare v1 v2

                _ ->
                    LT


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
