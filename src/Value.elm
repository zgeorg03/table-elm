module Value
    exposing
        ( Value(..)
        , ValueType(..)
        , getDefaultValueFromType
        , getDefaultValue
        , compare
        , toString
        , toCsv
        )

{-| This module wraps all the possible types a table cell can have

# Basics
@docs Value,ValueType,getDefaultValueFromType, getDefaultValue, compare, toString,  toCsv

-}

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
    | E


{-| This type holds the possible types for a table cell
-}
type ValueType
    = IntType
    | StringType
    | DateType
    | FloatType
    | BoolType
    | EmptyType


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

        EmptyType ->
            E


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

        E ->
            E


{-| Convert to Csv
-}
toCsv : Value -> String
toCsv value =
    "\"" ++ (toString value) ++ "\""


{-| String representation of a value
-}
toString : Value -> String
toString value =
    case value of
        E ->
            "Empty"

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
                    if (Date.day date) < 10 then
                        "0" ++ (Date.day date |> Basics.toString)
                    else
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

                secs =
                    if (Date.second date) < 10 then
                        "0" ++ (Date.second date |> Basics.toString)
                    else
                        Date.second date |> Basics.toString
            in
                year ++ "-" ++ month ++ "-" ++ day ++ " " ++ hours ++ ":" ++ mins ++ ":" ++ secs


getDateFromInt : Int -> Date
getDateFromInt timestamp =
    (1000 * timestamp) |> toFloat |> Date.fromTime


{-| Comparator for Value
  **TODO** Comparing different types of values for now returns LT
-}
compare : Value -> Value -> Order
compare a b =
    case a of
        E ->
            case b of
                _ ->
                    LT

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
                "01"

            Feb ->
                "02"

            Mar ->
                "03"

            Apr ->
                "04"

            May ->
                "05"

            Jun ->
                "06"

            Jul ->
                "07"

            Aug ->
                "08"

            Sep ->
                "09"

            Oct ->
                "10"

            Nov ->
                "11"

            Dec ->
                "12"
