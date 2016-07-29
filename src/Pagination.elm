module Pagination exposing (Model, Msg, init, update, view)

{-| Pagination module for the table

@docs Model, Msg, init, update, view
-}

import Html.App exposing (program)
import Html.Attributes exposing (class, value, selected, placeholder)
import Html.Events exposing (onClick)
import Html exposing (..)
import Array exposing (..)


{-| Model Description
-}
type alias Model =
    { totalEntries : Int
    , entriesInPage : Int
    , activeEntries : Int
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
            let
                activeEntries =
                    if (model.totalEntries < model.entriesInPage) then
                        model.totalEntries
                    else
                        model.entriesInPage
            in
                ( { model | currentPage = 0, activeEntries = activeEntries }, Cmd.none )

        LastPage ->
            let
                activeEntries =
                    model.totalEntries - ((model.totalPages - 1) * model.entriesInPage)
            in
                ( { model | currentPage = model.totalPages - 1, activeEntries = activeEntries }, Cmd.none )

        NextPage ->
            let
                nextPage =
                    if (model.currentPage + 1) < model.totalPages then
                        model.currentPage + 1
                    else
                        model.currentPage

                activeEntries =
                    if (nextPage == model.totalPages - 1) then
                        model.totalEntries - ((model.totalPages - 1) * model.entriesInPage)
                    else if (model.totalEntries < model.entriesInPage) then
                        model.totalEntries
                    else
                        model.entriesInPage
            in
                ( { model | currentPage = nextPage, activeEntries = activeEntries }, Cmd.none )

        PrevPage ->
            let
                nextPage =
                    if model.currentPage - 1 >= 0 then
                        model.currentPage - 1
                    else
                        model.currentPage

                activeEntries =
                    if (model.totalEntries < model.entriesInPage) then
                        model.totalEntries
                    else
                        model.entriesInPage
            in
                ( { model | currentPage = nextPage, activeEntries = activeEntries }, Cmd.none )

        GotoPage nextPage ->
            let
                activeEntries =
                    if (nextPage == model.totalPages - 1) then
                        model.totalEntries - ((model.totalPages - 1) * model.entriesInPage)
                    else if (model.totalEntries < model.entriesInPage) then
                        model.totalEntries
                    else
                        model.entriesInPage
            in
                ( { model | currentPage = nextPage, activeEntries = activeEntries }, Cmd.none )

        ChangeEntriesInPage entries ->
            let
                totalPages =
                    ceiling ((toFloat model.totalEntries) / (toFloat entries))

                pages =
                    (Array.initialize totalPages (\n -> n))

                activeEntries =
                    if (model.totalEntries < entries) then
                        model.totalEntries
                    else
                        entries
            in
                ( { model
                    | currentPage = 0
                    , pages = pages
                    , entriesInPage = entries
                    , totalPages = totalPages
                    , activeEntries = activeEntries
                  }
                , Cmd.none
                )


{-| Possible Actions
-}
type Msg
    = NoOp
    | FirstPage
    | LastPage
    | NextPage
    | PrevPage
    | GotoPage Int
    | ChangeEntriesInPage Int


{-| View of the model. Default visible entries is 10.
-}
view : Model -> Html Msg
view model =
    let
        base =
            model.currentPage * model.entriesInPage
    in
        div [ class "row" ]
            [ div [ class "col-md-4" ]
                [ text "Show  "
                , select []
                    [ option [ onClick (ChangeEntriesInPage 1) ] [ text "1" ]
                    , option [ onClick (ChangeEntriesInPage 2) ] [ text "2" ]
                    , option [ onClick (ChangeEntriesInPage 5) ] [ text "5" ]
                    , option [ selected True, onClick (ChangeEntriesInPage 10) ] [ text "10" ]
                    , option [ onClick (ChangeEntriesInPage 25) ] [ text "25" ]
                    , option [ onClick (ChangeEntriesInPage 50) ] [ text "50" ]
                    , option [ onClick (ChangeEntriesInPage 100) ] [ text "100" ]
                    , option [ onClick (ChangeEntriesInPage 200) ] [ text "200" ]
                    ]
                , text "  Entries"
                ]
            , nav [ class "col-md-4" ]
                [ ul [ class "pagination" ]
                    (showPages model)
                ]
            , div [ class "col-md-4" ]
                [ text
                    (if model.totalEntries == 0 then
                        "No entries found"
                     else
                        ("Showing "
                            ++ (toString (base + 1))
                            ++ " to "
                            ++ (toString (base + model.activeEntries))
                            ++ " of "
                            ++ (toString model.totalEntries)
                            ++ " Entries"
                        )
                    )
                ]
            ]


{-| Show the pagination buttons
-}
showPages : Model -> List (Html Msg)
showPages model =
    let
        firstPrevious =
            [ li [] [ a [ onClick FirstPage ] [ text "First" ] ]
            , li [] [ a [ onClick PrevPage ] [ text "Previous" ] ]
            ]

        startPos =
            if (model.totalPages <= 5) then
                0
            else if (model.currentPage + 2 <= 3) then
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


{-| Init Model
-}
init : Int -> Int -> Model
init records visible =
    let
        activeEntries =
            if (records < visible) then
                records
            else
                visible

        nOfPages : Int
        nOfPages =
            ceiling ((toFloat records) / (toFloat visible))
    in
        Model
            records
            visible
            activeEntries
            nOfPages
            0
            (Array.initialize nOfPages (\n -> n))


{-| Initialization with Cmd

-}
initCmd : Int -> Int -> ( Model, Cmd Msg )
initCmd records visible =
    ( init records visible, Cmd.none )


main : Program Never
main =
    program
        { init = initCmd 5 5
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }
