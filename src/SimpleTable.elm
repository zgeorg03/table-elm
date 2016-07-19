module SimpleTable exposing (..)

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
    , records : List IdRecord
    , size : Int
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

        len =
            length arrayHeaders

        indexedListHeaders =
            toIndexedList arrayHeaders

        indexedListRecords =
            toIndexedList arrayRecords
    in
        ( Model (List.map initHeaderHelper indexedListHeaders)
            (List.map initRecordHelper indexedListRecords)
            len
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
                    List.unzip (List.map (updateRecordHelp id msg) model.records)
            in
                ( { model | records = newRecords }, Cmd.batch cmds )

        Sort int ->
            ( model, Cmd.none )


sort : Int -> Model -> Model
sort index model =
    model


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
                        (viewRecords model.records)

                Just error ->
                    tr [] [ text error ]
            ]
        ]


viewRecords : List (IdRecord) -> List (Html Msg)
viewRecords list =
    List.map viewRecord list


viewRecord : IdRecord -> Html Msg
viewRecord { id, model } =
    (App.map (RecordMsg id) (Record.view model))


viewRecordCell : Value -> Html Msg
viewRecordCell value =
    td [] [ text (Value.toString value) ]


viewHeader : IdHeader -> Html Msg
viewHeader { id, model } =
    App.map (HeaderMsg id) (Header.view model)


headers : List Header.Model
headers =
    [ { title = "ID", state = Original, type' = IntType }
    , { title = "Name", state = Original, type' = StringType }
    , { title = "Surname", state = Original, type' = StringType }
    , { title = "DateOfBirth", state = Original, type' = DateType }
    ]


records : List Record.Model
records =
    [ Record.init
        [ Cell.init (I 955275)
        , Cell.init (S "Zacharias")
        , Cell.init (S "Georgiou")
        , Cell.init (D 756216594)
        ]
    , Record.init
        [ Cell.init (I 123456)
        , Cell.init (S "Andreas")
        , Cell.init (S "Andreou")
        , Cell.init (D 726216594)
        ]
    , Record.init
        [ Cell.init (I 123456)
        , Cell.init (S "Andreas")
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


main : Program Never
main =
    program
        { init = init headers records
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }
