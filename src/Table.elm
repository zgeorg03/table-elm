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
import Pagination exposing (..)
import Search exposing (..)
import String
import Html exposing (..)
import Html.App as App exposing (program)
import Html.Attributes exposing (class, value, style, disabled)


--import Html.Events exposing (onClick, onInput)

import Array exposing (..)


--import String
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
    , pagination : Pagination.Model
    , search : Search.Model
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

        nextEnabled =
            if recordsLen > visibleRecords then
                True
            else
                False

        searchableRecords =
            getSearchableRecords records
    in
        ( Model title
            (List.map initHeaderHelper indexedListHeaders)
            (Array.map initRecordHelper (indexedListRecords |> fromList))
            (Array.initialize recordsLen (\n -> n) |> Array.toList)
            recordsLen
            headersLen
            0
            (Pagination.init recordsLen visibleRecords)
            (Search.init searchableRecords)
            Nothing
        , Cmd.none
        )


resetHeaders : Model -> Model
resetHeaders table =
    let
        headers : List IdHeader
        headers =
            List.map (resetIdHeader) table.headers
    in
        { table | headers = headers }


resetIdHeader : IdHeader -> IdHeader
resetIdHeader idHeader =
    let
        header =
            idHeader.model
    in
        IdHeader idHeader.id (Header.reset header)


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
    | PaginationMsg Pagination.Msg
    | SearchMsg Search.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        PaginationMsg msg ->
            let
                ( pagination, cmds ) =
                    Pagination.update msg model.pagination

                base =
                    pagination.currentPage * pagination.entriesInPage
            in
                ( { model | pagination = pagination, base = base }, Cmd.map PaginationMsg cmds )

        SearchMsg msg ->
            let
                oldPagination =
                    model.pagination

                ( search, cmds ) =
                    Search.update msg model.search

                perm =
                    search.searchList

                pagination =
                    Pagination.init (List.length perm) oldPagination.entriesInPage
            in
                ( { model | search = search, permutation = perm, pagination = pagination }, Cmd.map SearchMsg cmds )

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


getSearchableRecords : List Record.Model -> List String
getSearchableRecords list =
    List.map (Record.toString) list


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
            let
                search =
                    model.search
            in
                if String.isEmpty search.value then
                    Array.initialize model.rows (\n -> n) |> Array.toList
                else
                    search.searchList

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
                    -- TODO Remove this
                    { id = 0, model = Cell.init (I 0) False }

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
                [ (App.map SearchMsg (Search.view model.search))
                , table [ class "table" ]
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
                [ (App.map PaginationMsg (Pagination.view model.pagination)) ]
            ]
        , div [ class "container" ] [ text (Basics.toString model.permutation) ]
        ]


viewRecordsPermutated : Model -> List (Html Msg)
viewRecordsPermutated model =
    let
        pagination =
            model.pagination

        perm =
            Array.slice model.base (model.base + pagination.entriesInPage) (model.permutation |> Array.fromList) |> Array.toList
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
        [ Cell.init (I 500012) True
        , Cell.init (S "Zacharias") True
        , Cell.init (S "Georgiou") True
        , Cell.init (D 756216594) True
        , Cell.init (B True) True
        ]
    , Record.init
        [ Cell.init (I 503212) True
        , Cell.init (S "Andreas") True
        , Cell.init (S "Mariou") True
        , Cell.init (D 726216594) True
        , Cell.init (B False) True
        ]
    , Record.init
        [ Cell.init (I 123456) True
        , Cell.init (S "Maria") True
        , Cell.init (S "Andreou") True
        , Cell.init (D 726216594) True
        , Cell.init (B True) True
        ]
    , Record.init
        [ Cell.init (I 123456) True
        , Cell.init (S "Chris") True
        , Cell.init (S "Petrou") True
        , Cell.init (D 644349992) True
        , Cell.init (B False) True
        ]
    , Record.init
        [ Cell.init (I 235409) True
        , Cell.init (S "Costas") True
        , Cell.init (S "Costa") True
        , Cell.init (D 944349992) False
        , Cell.init (B False) True
        ]
    , Record.init
        [ Cell.init (I 524202) True
        , Cell.init (S "Andri") True
        , Cell.init (S "Andreou") True
        , Cell.init (D 1244349992) True
        , Cell.init (B False) True
        ]
    , Record.init
        [ Cell.init (I 525400) True
        , Cell.init (S "Maria") True
        , Cell.init (S "Christou") True
        , Cell.init (D 244349992) True
        , Cell.init (B False) True
        ]
    , Record.init
        [ Cell.init (I 525500) True
        , Cell.init (S "Marios") True
        , Cell.init (S "Georgiou") False
        , Cell.init (D 344349992) True
        , Cell.init (B True) True
        ]
    , Record.init
        [ Cell.init (I 595500) True
        , Cell.init (S "Marios") True
        , Cell.init (S "Mariou") True
        , Cell.init (D 344349992) True
        , Cell.init (B True) True
        ]
    , Record.init
        [ Cell.init (I 525500) True
        , Cell.init (S "Marios") True
        , Cell.init (S "Georgiou") True
        , Cell.init (D 344349992) True
        , Cell.init (B True) True
        ]
    , Record.init
        [ Cell.init (I 405500) True
        , Cell.init (S "Giorgos") True
        , Cell.init (S "Antreou") True
        , Cell.init (D 344049992) True
        , Cell.init (B True) True
        ]
    , Record.init
        [ Cell.init (I 952003) True
        , Cell.init (S "Emily") True
        , Cell.init (S "Antreou") True
        , Cell.init (D 344049992) True
        , Cell.init (B True) True
        ]
    ]
