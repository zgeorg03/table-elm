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
import Html.Events exposing (onClick)
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
    , permutation : Array Int
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

        indexedArrayRecords =
            fromList indexedListRecords
    in
        ( Model (List.map initHeaderHelper indexedListHeaders)
            (Array.map initRecordHelper indexedArrayRecords)
            (Array.initialize headersLen (\n -> n))
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
    | Sort Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        HeaderMsg id msg ->
            let
                ( newHeaders, cmds ) =
                    List.unzip (List.map (updateHeaderHelp id msg) model.headers)
            in
                ( { model | headers = newHeaders }, Cmd.batch cmds )

        RecordMsg id msg ->
            let
                ( newRecords, cmds ) =
                    List.unzip (Array.map (updateRecordHelp id msg) model.records |> toList)
            in
                ( { model | records = Array.fromList newRecords }, Cmd.batch cmds )

        Sort col ->
            let
                newModel =
                    sort col model
            in
                ( newModel, Cmd.none )


sort : Int -> Model -> Model
sort index model =
    let
        newPerm =
            getPermutation index model
    in
        { model | permutation = newPerm }


getPermutation : Int -> Model -> Array Int
getPermutation col model =
    let
        perm =
            Array.toList model.permutation

        newPerm : Array Int
        newPerm =
            --List.sortWith compareValue (List.map (wrapper col model) perm) |> Array.fromList
            --List.sortBy (wrapper col model) perm |> Array.fromList
            List.sort perm |> List.reverse |> Array.fromList
    in
        newPerm


wrapper : Int -> Model -> Int -> Value
wrapper col model row =
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

        record : Record.Model
        record =
            idRecord.model

        mayIdCell : Maybe { id : Int, model : Cell.Model }
        mayIdCell =
            get col record

        idCell : { id : Int, model : Cell.Model }
        idCell =
            case mayIdCell of
                Nothing ->
                    { id = 0, model = Cell.init (I 0) }

                Just cell ->
                    cell

        cell : Cell.Model
        cell =
            idCell.model
    in
        cell.value


compareValue : Value -> Value -> Order
compareValue a b =
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
        , text (Basics.toString model.permutation)
        , button [ onClick (Sort 1) ] [ text "Sort" ]
        ]


viewRecordsPermutated : Model -> List (Html Msg)
viewRecordsPermutated model =
    Array.map (\x -> Array.get x model.records |> viewRecordPermutated) model.permutation |> Array.toList


viewRecordPermutated : Maybe IdRecord -> Html Msg
viewRecordPermutated res =
    case res of
        Nothing ->
            div [] []

        Just record ->
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
