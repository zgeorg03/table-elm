module Pagination exposing (Model, Msg, init, update, view)

{-| Pagination module for the table

@docs Model, Msg, init, update, view
-}

import ExternalCSS exposing (stylesheet)
import Html.App exposing (program)
import Html.Attributes exposing (class, value, selected)
import Html.Events exposing (onClick)
import Html exposing (..)
import Array exposing (..)


{-| Model
-}
type alias Model =
    { totalEntries : Int
    , entriesInPage : Int
    , totalPages : Int
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

        FirstPage ->
            ( { model | currentPage = 0 }, Cmd.none )

        LastPage ->
            ( { model | currentPage = model.totalPages - 1 }, Cmd.none )

        NextPage ->
            let
                nextPage =
                    if (model.currentPage + 1) < model.totalPages then
                        model.currentPage + 1
                    else
                        model.currentPage
            in
                ( { model | currentPage = nextPage }, Cmd.none )

        PrevPage ->
            let
                nextPage =
                    if model.currentPage - 1 >= 0 then
                        model.currentPage - 1
                    else
                        model.currentPage
            in
                ( { model | currentPage = nextPage }, Cmd.none )

        GotoPage nextPage ->
            ( { model | currentPage = nextPage }, Cmd.none )

        ChangeEntriesInPage entries ->
            let
                totalPages =
                    ceiling ((toFloat model.totalEntries) / (toFloat entries))

                pages =
                    (Array.initialize totalPages (\n -> n))
            in
                ( { model | currentPage = 0, pages = pages, entriesInPage = entries, totalPages = totalPages }, Cmd.none )


{-| Msg
-}
type Msg
    = NoOp
    | FirstPage
    | LastPage
    | NextPage
    | PrevPage
    | GotoPage Int
    | ChangeEntriesInPage Int


{-| View of the model
-}
view : Model -> Html Msg
view model =
    div [ class "row" ]
        [ stylesheet
        , div [ class "col-md-2" ]
            [ text "Show  "
            , select []
                [ option [ onClick (ChangeEntriesInPage 1) ] [ text "1" ]
                , option [ onClick (ChangeEntriesInPage 2) ] [ text "2" ]
                , option [ selected True, onClick (ChangeEntriesInPage 5) ] [ text "5" ]
                , option [ onClick (ChangeEntriesInPage 10) ] [ text "10" ]
                , option [ onClick (ChangeEntriesInPage 25) ] [ text "25" ]
                , option [ onClick (ChangeEntriesInPage 50) ] [ text "50" ]
                , option [ onClick (ChangeEntriesInPage 100) ] [ text "100" ]
                , option [ onClick (ChangeEntriesInPage 200) ] [ text "200" ]
                ]
            , text "  Entries"
            ]
        , nav [ class "col-md-10" ]
            [ ul [ class "pagination" ]
                (showPages model)
            ]
        ]


showPages : Model -> List (Html Msg)
showPages model =
    let
        firstPrevious =
            [ li [] [ a [ onClick FirstPage ] [ text "First" ] ]
            , li [] [ a [ onClick PrevPage ] [ text "Previous" ] ]
            ]

        startPos =
            if (model.currentPage + 2 <= 3) then
                0
            else if (model.currentPage + 2 >= model.totalPages) then
                model.totalPages - 5
            else
                model.currentPage - 2

        viewport =
            Array.slice startPos (startPos + 5) model.pages

        inner =
            Array.map (showPage model.currentPage) viewport |> Array.toList

        nextLast =
            [ li [] [ a [ onClick NextPage ] [ text "Next" ] ]
            , li [] [ a [ onClick LastPage ] [ text "Last" ] ]
            ]
    in
        firstPrevious ++ inner ++ nextLast


showPage : Int -> Int -> Html Msg
showPage currentPage id =
    if id == currentPage then
        li [ class "active" ] [ a [] [ text (Basics.toString (id + 1)) ] ]
    else
        li [] [ a [ onClick (GotoPage id) ] [ text (Basics.toString (id + 1)) ] ]


{-| Initialization

-}
init : Int -> Int -> ( Model, Cmd Msg )
init records visible =
    let
        nOfPages : Int
        nOfPages =
            ceiling ((toFloat records) / (toFloat visible))
    in
        ( Model
            records
            visible
            nOfPages
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
