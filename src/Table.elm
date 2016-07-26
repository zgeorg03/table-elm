module Table exposing (Model, Msg, init, initHeaders, update, view)

{-|
  This module implements a simple table

@docs Model, Msg, init, initHeaders, update, view

-}

import ExternalCSS exposing (..)
import Header exposing (..)
import Record exposing (..)
import Cell exposing (..)
import Value exposing (..)
import Pagination exposing (..)
import Search exposing (..)
import Http
import String
import Html exposing (..)
import Html.App as App exposing (program)
import Html.Attributes exposing (class, value, style, disabled, href, downloadAs)


--import Html.Events exposing (onClick, onInput)

import Array exposing (..)


type alias IdHeader =
    { id : Int
    , model : Header.Model
    }


type alias IdRecord =
    { id : Int
    , model : Record.Model
    }


{-| Model

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
    , csv : String
    , error : Maybe String
    }



--INIT


{-| Initialization of the Table only with Headers

-}
initHeaders : String -> List Header.Model -> Bool -> Model
initHeaders title headers auto =
    let
        visibleRecords =
            10

        arrayHeaders =
            fromList headers

        headersLen =
            length arrayHeaders

        indexedListHeaders =
            toIndexedList arrayHeaders

        searchableRecords =
            getRecordsForSearch []
    in
        Model title
            (List.map initHeaderHelper indexedListHeaders)
            Array.empty
            []
            0
            headersLen
            0
            (Pagination.init 0 visibleRecords)
            (Search.init auto searchableRecords)
            ""
            Nothing


{-| Initialization of the Table with Headers and Records

-}
init : String -> List Header.Model -> List Record.Model -> Bool -> Model
init title headers records auto =
    let
        visibleRecords =
            10

        perm =
            (Array.initialize recordsLen (\n -> n) |> Array.toList)

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

        searchableRecords =
            getRecordsForSearch records

        csv =
            getCsv headers records
    in
        Model title
            (List.map initHeaderHelper indexedListHeaders)
            (Array.map initRecordHelper (indexedListRecords |> fromList))
            perm
            recordsLen
            headersLen
            0
            (Pagination.init recordsLen visibleRecords)
            (Search.init auto searchableRecords)
            csv
            Nothing


initCmd : String -> List Header.Model -> List Record.Model -> Bool -> ( Model, Cmd Msg )
initCmd title headers records auto =
    ( init title headers records auto, Cmd.none )


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


{-| Possible actions
-}
type Msg
    = HeaderMsg Int Header.Msg
    | RecordMsg Int Record.Msg
    | PaginationMsg Pagination.Msg
    | SearchMsg Search.Msg


{-| Controller
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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

                csv =
                    updateCsv model.headers model.records perm
            in
                ( { model | csv = csv, base = 0, search = search, permutation = perm, pagination = pagination }, Cmd.map SearchMsg cmds )

        HeaderMsg id msg ->
            let
                ( newHeaders, cmds ) =
                    List.unzip (List.map (updateHeaderHelp id msg) model.headers)

                -- Get current state
                state =
                    getHeaderState id newHeaders

                newModel =
                    sort id state model

                csv =
                    updateCsv newModel.headers newModel.records newModel.permutation
            in
                ( { newModel | csv = csv, headers = newHeaders }, Cmd.batch cmds )

        RecordMsg id msg ->
            let
                ( newRecords, cmds ) =
                    List.unzip (Array.map (updateRecordHelp id msg) model.records |> toList)
            in
                ( { model | records = newRecords |> fromList }, Cmd.batch cmds )


getCsv : List Header.Model -> List Record.Model -> String
getCsv headers records =
    let
        h =
            List.map (Header.toCsv) headers |> String.join (String.fromChar ',')

        r =
            List.map (Record.toCsv) records |> String.join (String.fromChar '\n')
    in
        h ++ (String.fromChar '\n') ++ r


updateCsv : List IdHeader -> Array IdRecord -> List Int -> String
updateCsv headers records perm =
    let
        h =
            List.map (\x -> x.model |> Header.toCsv) headers |> String.join (String.fromChar ',')

        r =
            List.map (\x -> Array.get x records |> toRecord |> Record.toCsv) perm |> String.join (String.fromChar '\n')
    in
        h ++ (String.fromChar '\n') ++ r


toRecord : Maybe IdRecord -> Record.Model
toRecord res =
    case res of
        Nothing ->
            Record.init []

        Just record ->
            record.model


getRecordsForSearch : List Record.Model -> List String
getRecordsForSearch list =
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


getCompositePermuation : Int -> State -> Model -> List Int -> List Int
getCompositePermuation col state model f =
    let
        slices =
            getSlices col model 0 f
    in
        slices


getSlices : Int -> Model -> Int -> List Int -> List Int
getSlices col model index list =
    case list of
        [] ->
            []

        x1 :: rest ->
            let
                x2 =
                    case rest of
                        [] ->
                            x1 + 1

                        p :: rest2 ->
                            p
            in
                if (areTheSame col model x1 x2) then
                    getSlices col model (index + 1) rest
                else
                    index :: getSlices col model (index + 1) rest


areTheSame : Int -> Model -> Int -> Int -> Bool
areTheSame col model id1 id2 =
    let
        v1 =
            getIdRecord id1 model |> getRecord |> getValue col

        v2 =
            getIdRecord id2 model |> getRecord |> getValue col
    in
        case Value.compare v1 v2 of
            EQ ->
                True

            _ ->
                False


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
            Array.get row records

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


{-| View of the model
-}
view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ (stylesheet)
        , div [ class "panel panel-primary" ]
            [ div [ class "panel-heading" ] [ text model.title ]
            , div [ class "panel-body" ]
                [ div [ class "row" ]
                    [ div [ class "col-md-10" ] [ (App.map SearchMsg (Search.view model.search)) ]
                    , div [ class "col-md-2" ] [ a [ href ("data:text/csv;charset=utf-8," ++ (Http.uriEncode model.csv)), downloadAs "table.csv" ] [ span [ class "glyphicon glyphicon-download-alt" ] [] ] ]
                    ]
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
        , div [] [ text (Basics.toString (getCompositePermuation 0 Ascending model model.permutation)) ]
        ]


recordsToString : Array IdRecord -> String
recordsToString array =
    Array.map (\x -> x.model |> Record.toString) array |> Array.toList |> String.join "#"


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
        { init = initCmd "Example table" dummyHeaders dummyRecords True
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
