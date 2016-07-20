module Table exposing (Model, init)

{-|
  This module implements a simple table

@docs Model, init

-}

import ExternalCSS exposing (..)
import Header exposing (..)
import Record exposing (..)
import Cell exposing (..)
import Value exposing (..)
import Html exposing (..)
import Html.App as App exposing (program)
import Html.Attributes exposing (class, value, style, disabled)
import Html.Events exposing (onClick, onInput)
import Array exposing (..)
import String


--MODEL


type alias IdHeader =
    { id : Int
    , model : Header.Model
    }


type alias IdRecord =
    { id : Int
    , model : Record.Model
    }


{-| Model
  ##Arguments##
  1. **title** The title shown in the header

-}
type alias Model =
    { title : String
    , headers : List IdHeader
    , records : Array IdRecord
    , permutation : List Int
    , rows : Int
    , cols : Int
    , base : Int
    , visibleRecords : Int
    , activeRecords : Int
    , prevEnabled : Bool
    , nextEnabled : Bool
    , error : Maybe String
    }



--INIT


{-| Initialization of the Table

-}
init : String -> List Header.Model -> List Record.Model -> ( Model, Cmd Msg )
init title headers records =
    let
        visibleRecords =
            5

        arrayHeaders =
            fromList headers

        arrayRecords =
            fromList records

        headersLen =
            length arrayHeaders

        recordsLen =
            length arrayRecords

        indexedListHeaders =
            toIndexedList arrayHeaders

        indexedListRecords =
            toIndexedList arrayRecords

        activeRecords =
            if visibleRecords > recordsLen then
                recordsLen
            else
                visibleRecords

        nextEnabled =
            if recordsLen > visibleRecords then
                True
            else
                False
    in
        ( Model title
            (List.map initHeaderHelper indexedListHeaders)
            (Array.map initRecordHelper (indexedListRecords |> fromList))
            (Array.initialize recordsLen (\n -> n) |> Array.toList)
            recordsLen
            headersLen
            0
            visibleRecords
            activeRecords
            False
            nextEnabled
            Nothing
        , Cmd.none
        )


initRecordHelper : ( Int, Record.Model ) -> IdRecord
initRecordHelper ( i, model ) =
    IdRecord i (model)


initHeaderHelper : ( Int, Header.Model ) -> IdHeader
initHeaderHelper ( i, model ) =
    IdHeader i (model)



--UPDATE


type Msg
    = NoOp
    | HeaderMsg Int Header.Msg
    | RecordMsg Int Record.Msg
    | IncrementBase
    | DecrementBase
    | ChangeVisibleRecords String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        HeaderMsg id msg ->
            let
                ( newHeaders, cmds ) =
                    List.unzip (List.map (updateHeaderHelp id msg) model.headers)

                -- Get current state
                state =
                    getHeaderState id newHeaders

                newModel =
                    sort id state model
            in
                ( { newModel | headers = newHeaders }, Cmd.batch cmds )

        RecordMsg id msg ->
            let
                ( newRecords, cmds ) =
                    List.unzip (Array.map (updateRecordHelp id msg) model.records |> toList)
            in
                ( { model | records = newRecords |> fromList }, Cmd.batch cmds )

        IncrementBase ->
            let
                activeRecords =
                    if model.rows - (model.visibleRecords + model.base) < model.visibleRecords then
                        model.rows - (model.visibleRecords + model.base)
                    else
                        model.visibleRecords

                newNext =
                    if (activeRecords < model.visibleRecords) then
                        False
                    else
                        True

                newBase =
                    if (model.rows - (model.visibleRecords + model.base)) <= 0 then
                        model.base
                    else
                        model.base + model.visibleRecords

                newPrev =
                    if (newBase - model.visibleRecords) < 0 then
                        False
                    else
                        True
            in
                ( { model | prevEnabled = newPrev, nextEnabled = newNext, base = newBase, activeRecords = activeRecords }, Cmd.none )

        DecrementBase ->
            let
                activeRecords =
                    if (model.rows - model.base) + model.visibleRecords < model.visibleRecords then
                        model.rows - (model.visibleRecords + model.base)
                    else
                        model.visibleRecords

                newPrev =
                    if (model.base - model.visibleRecords) <= 0 then
                        False
                    else
                        True

                newBase =
                    if (model.base - model.visibleRecords) < 0 then
                        model.base
                    else
                        model.base - model.visibleRecords

                newNext =
                    if (newBase - model.visibleRecords) > 0 then
                        False
                    else
                        True
            in
                ( { model | nextEnabled = newNext, prevEnabled = newPrev, base = newBase, activeRecords = activeRecords }, Cmd.none )

        ChangeVisibleRecords str ->
            let
                ( base, visibleRecords ) =
                    case String.toInt str of
                        Ok n ->
                            ( 0, n )

                        Err _ ->
                            ( model.base, model.visibleRecords )

                activeRecords =
                    if (visibleRecords > model.rows) then
                        model.rows
                    else if (model.rows - model.base) + visibleRecords < visibleRecords then
                        model.rows - (visibleRecords + model.base)
                    else
                        visibleRecords

                newNext =
                    if (model.rows - (visibleRecords + base)) <= 0 then
                        False
                    else
                        True

                newPrev =
                    if (base - visibleRecords) < 0 then
                        False
                    else
                        True
            in
                ( { model | visibleRecords = visibleRecords, nextEnabled = newNext, prevEnabled = newPrev, base = base, activeRecords = activeRecords }, Cmd.none )


getHeaderState : Int -> List IdHeader -> State
getHeaderState col idHeaders =
    let
        array =
            Array.fromList idHeaders

        mayIdHeader =
            Array.get col array

        idHeader =
            case mayIdHeader of
                Nothing ->
                    --TODO Fix this
                    { id = 0, model = Header.Model "" Original IntType }

                Just s ->
                    s

        header =
            idHeader.model
    in
        header.state


sort : Int -> State -> Model -> Model
sort col state model =
    { model | permutation = getNewPermutation col state model }


getNewPermutation : Int -> State -> Model -> List Int
getNewPermutation col state model =
    case state of
        Original ->
            Array.initialize model.rows (\n -> n) |> Array.toList

        Ascending ->
            List.sortWith (sortForPermutation model col) model.permutation

        Descending ->
            List.sortWith (\i -> (\j -> (sortForPermutation model col j i))) model.permutation


sortForPermutation : Model -> Int -> Int -> Int -> Order
sortForPermutation model col id1 id2 =
    let
        rec1 =
            getIdRecord id1 model

        rec2 =
            getIdRecord id2 model
    in
        sortIdRecord col rec1 rec2


sortIdRecord : Int -> IdRecord -> IdRecord -> Order
sortIdRecord col rec1 rec2 =
    Value.compare (getRecord rec1 |> getValue col) (getRecord rec2 |> getValue col)


getIdRecord : Int -> Model -> IdRecord
getIdRecord row model =
    let
        records : Array IdRecord
        records =
            model.records

        mayIdRecord : Maybe IdRecord
        mayIdRecord =
            get row records

        idRecord : IdRecord
        idRecord =
            case mayIdRecord of
                Nothing ->
                    { id = 0, model = Record.init [] }

                Just record ->
                    record
    in
        idRecord


getRecord : IdRecord -> Record.Model
getRecord idRecord =
    idRecord.model


getValue : Int -> Record.Model -> Value
getValue index record =
    let
        mayIdCell =
            get index record

        idCell : { id : Int, model : Cell.Model }
        idCell =
            case mayIdCell of
                Nothing ->
                    { id = 0, model = Cell.init (I 0) }

                Just cell ->
                    cell

        cell =
            idCell.model
    in
        cell.value


updateHeaderHelp : Int -> Header.Msg -> IdHeader -> ( IdHeader, Cmd Msg )
updateHeaderHelp id msg header =
    if header.id /= id then
        let
            hModel =
                header.model

            newModel =
                { hModel | state = Original }
        in
            ( { header | model = newModel }, Cmd.none )
    else
        let
            ( newModel, cmds ) =
                Header.update msg header.model
        in
            ( IdHeader id newModel, Cmd.map (HeaderMsg id) cmds )


updateRecordHelp : Int -> Record.Msg -> IdRecord -> ( IdRecord, Cmd Msg )
updateRecordHelp id msg record =
    if record.id /= id then
        ( record, Cmd.none )
    else
        let
            ( newModel, cmds ) =
                Record.update msg record.model
        in
            ( IdRecord id newModel, Cmd.map (RecordMsg id) cmds )


view : Model -> Html Msg
view model =
    div []
        [ (stylesheet)
        , div [ class "panel panel-primary" ]
            [ div [ class "panel-heading" ] [ text model.title ]
            , div [ class "panel-body" ]
                [ table [ class "table" ]
                    [ thead []
                        [ tr [] (List.map viewHeader model.headers)
                        ]
                    , case model.error of
                        Nothing ->
                            tbody []
                                (viewRecordsPermutated model)

                        Just error ->
                            tr [] [ text error ]
                    ]
                ]
            , div [ class "panel-footer" ]
                [ div []
                    [ text "Show  "
                    , select []
                        [ option [ onClick (ChangeVisibleRecords "2") ] [ text "2" ]
                        , option [ onClick (ChangeVisibleRecords "5") ] [ text "5" ]
                        , option [ onClick (ChangeVisibleRecords "10") ] [ text "10" ]
                        , option [ onClick (ChangeVisibleRecords "25") ] [ text "25" ]
                        , option [ onClick (ChangeVisibleRecords "50") ] [ text "50" ]
                        , option [ onClick (ChangeVisibleRecords "100") ] [ text "100" ]
                        ]
                    , text "  Entries"
                    ]
                , div []
                    [ nav []
                        [ ul [ class "pagination" ]
                            [ li [ onClick DecrementBase ] [ button [ disabled (not model.prevEnabled) ] [ a [] [ text "Previous" ] ] ]
                            , li [ onClick IncrementBase ] [ button [ disabled (not model.nextEnabled) ] [ a [] [ text "Next" ] ] ]
                            ]
                        ]
                    ]
                , div []
                    [ text ("Showing " ++ (Basics.toString (model.base + 1)) ++ " to " ++ (Basics.toString (model.base + model.activeRecords)) ++ " of " ++ (Basics.toString model.rows) ++ " entries")
                    ]
                ]
            ]
        , div [ class "container" ] [ text (Basics.toString model.permutation) ]
        , div [ class "container" ] [ text (Basics.toString model.base) ]
        ]


viewRecordsPermutated : Model -> List (Html Msg)
viewRecordsPermutated model =
    let
        perm =
            Array.slice model.base (model.base + model.visibleRecords) (model.permutation |> Array.fromList) |> Array.toList
    in
        List.map (\x -> Array.get x model.records |> viewRecordPermutated) perm


viewRecordPermutated : Maybe IdRecord -> Html Msg
viewRecordPermutated res =
    case res of
        Nothing ->
            tr [] []

        Just record ->
            (App.map (RecordMsg record.id) (Record.view record.model))


viewRecords : Model -> List (Html Msg)
viewRecords model =
    Array.map viewRecord model.records |> toList


viewRecord : IdRecord -> Html Msg
viewRecord record =
    (App.map (RecordMsg record.id) (Record.view record.model))


viewHeader : IdHeader -> Html Msg
viewHeader { id, model } =
    App.map (HeaderMsg id) (Header.view model)


main : Program Never
main =
    program
        { init = init "Example table" dummyHeaders dummyRecords
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }


dummyHeaders : List Header.Model
dummyHeaders =
    [ { title = "ID", state = Original, type' = IntType }
    , { title = "Name", state = Original, type' = StringType }
    , { title = "Surname", state = Original, type' = StringType }
    , { title = "DateOfBirth", state = Original, type' = DateType }
    , { title = "Rich", state = Original, type' = BoolType }
    ]


dummyRecords : List Record.Model
dummyRecords =
    [ Record.init
        [ Cell.init (I 500012)
        , Cell.init (S "Zacharias")
        , Cell.init (S "Georgiou")
        , Cell.init (D 756216594)
        , Cell.init (B True)
        ]
    , Record.init
        [ Cell.init (I 503212)
        , Cell.init (S "Andreas")
        , Cell.init (S "Mariou")
        , Cell.init (D 726216594)
        , Cell.init (B False)
        ]
    , Record.init
        [ Cell.init (I 123456)
        , Cell.init (S "Maria")
        , Cell.init (S "Andreou")
        , Cell.init (D 726216594)
        , Cell.init (B True)
        ]
    , Record.init
        [ Cell.init (I 123456)
        , Cell.init (S "Chris")
        , Cell.init (S "Petrou")
        , Cell.init (D 644349992)
        , Cell.init (B False)
        ]
    , Record.init
        [ Cell.init (I 235409)
        , Cell.init (S "Costas")
        , Cell.init (S "Costa")
        , Cell.init (D 944349992)
        , Cell.init (B False)
        ]
    , Record.init
        [ Cell.init (I 524202)
        , Cell.init (S "Andri")
        , Cell.init (S "Andreou")
        , Cell.init (D 1244349992)
        , Cell.init (B False)
        ]
    , Record.init
        [ Cell.init (I 525400)
        , Cell.init (S "Maria")
        , Cell.init (S "Christou")
        , Cell.init (D 244349992)
        , Cell.init (B False)
        ]
    , Record.init
        [ Cell.initEditable (I 525500)
        , Cell.init (S "Marios")
        , Cell.init (S "Georgiou")
        , Cell.init (D 344349992)
        , Cell.init (B True)
        ]
    , Record.init
        [ Cell.init (I 595500)
        , Cell.init (S "Marios")
        , Cell.init (S "Mariou")
        , Cell.init (D 344349992)
        , Cell.init (B True)
        ]
    , Record.init
        [ Cell.init (I 525500)
        , Cell.init (S "Marios")
        , Cell.init (S "Georgiou")
        , Cell.init (D 344349992)
        , Cell.init (B True)
        ]
    , Record.init
        [ Cell.init (I 405500)
        , Cell.init (S "Giorgos")
        , Cell.init (S "Antreou")
        , Cell.init (D 344049992)
        , Cell.init (B True)
        ]
    ]
