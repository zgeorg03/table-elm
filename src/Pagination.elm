module Pagination exposing (..)

{-| Pagination module for the table
-}

import ExternalCSS exposing (stylesheet)
import Html.App exposing (program)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Html exposing (..)
import Array exposing (..)


type alias Model =
    { totalEntries : Int
    , entriesInPage : Int
    , prevEnabled : Bool
    , nextEnabled : Bool
    , currentPage : Int
    , pages : Array Int
    }


{-| Update Method

-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


type Msg
    = NoOp


{-| View of the model
-}
view : Model -> Html Msg
view model =
    div []
        [ stylesheet
        , div [ class "container" ] [ text (Basics.toString model) ]
        , nav []
            [ ul [ class "pagination" ]
                [ li [ onClick NoOp ] [ a [] [ text "First" ] ]
                , li [ onClick NoOp ] [ a [] [ text "Previous" ] ]
                , li [ onClick NoOp ] [ a [] [ text "Next" ] ]
                , li [ onClick NoOp ] [ a [] [ text "Last" ] ]
                ]
            ]
        ]


{-| Initialization

-}
init : Int -> Int -> ( Model, Cmd Msg )
init records visible =
    let
        nOfPages =
            records // visible
    in
        ( Model
            records
            visible
            True
            True
            0
            (Array.initialize nOfPages (\n -> n))
        , Cmd.none
        )


main : Program Never
main =
    program
        { init = init 10 5
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }
