module Table exposing (..)

{-|

-}

import ExternalCSS exposing (..)
import Header exposing (..)
import Record exposing (..)
import Cell exposing (..)
import Value exposing (..)
import Html exposing (..)
import Html.App as App exposing (program)
import Html.Attributes exposing (class, value)
import Array exposing (..)


--MODEL


type alias IdHeader =
    { id : Int
    , model : Header.Model
    }


type alias IdRecord =
    { id : Int
    , model : Record.Model
    }


type alias Model =
    { headers : List IdHeader
    , records : Array IdRecord
    , permutation : List Int
    , rows : Int
    , cols : Int
    , error : Maybe String
    }



--INIT


init : List Header.Model -> List Record.Model -> ( Model, Cmd Msg )
init headers records =
    let
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
    in
        ( Model (List.map initHeaderHelper indexedListHeaders)
            (Array.map initRecordHelper (indexedListRecords |> fromList))
            (Array.initialize headersLen (\n -> n) |> Array.toList)
            recordsLen
            headersLen
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
            List.sortWith (sortForPermutation model col) model.permutation |> List.reverse


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
        ( header, Cmd.none )
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
        , text (Basics.toString model.headers)
        , text (Basics.toString model.permutation)
        ]


viewRecordsPermutated : Model -> List (Html Msg)
viewRecordsPermutated model =
    List.map (\x -> Array.get x model.records |> viewRecordPermutated) model.permutation


viewRecordPermutated : Maybe IdRecord -> Html Msg
viewRecordPermutated res =
    case res of
        Nothing ->
            div [] []

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
        { init = init dummyHeaders dummyRecords
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
    ]


dummyRecords : List Record.Model
dummyRecords =
    [ Record.init
        [ Cell.init (I 500012)
        , Cell.init (S "Zacharias")
        , Cell.init (S "Georgiou")
        , Cell.init (D 756216594)
        ]
    , Record.init
        [ Cell.init (I 503212)
        , Cell.init (S "Andreas")
        , Cell.init (S "Mariou")
        , Cell.init (D 726216594)
        ]
    , Record.init
        [ Cell.init (I 123456)
        , Cell.init (S "Marias")
        , Cell.init (S "Andreou")
        , Cell.init (D 726216594)
        ]
    , Record.init
        [ Cell.init (I 123456)
        , Cell.init (S "Chris")
        , Cell.init (S "Petrou")
        , Cell.init (D 644349992)
        ]
    ]
