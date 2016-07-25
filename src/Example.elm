module Main exposing (..)

import Table exposing (..)
import Record exposing (..)
import Cell exposing (..)
import Value exposing (..)
import Header exposing (..)
import Http exposing (..)
import Html exposing (..)
import Html.App as App exposing (program)
import Html.Attributes exposing (placeholder, class)
import Html.Events exposing (onClick, onInput, on, keyCode)
import Task
import Json.Decode as Json exposing (..)
import Json.Decode.Extra exposing (..)


type alias Model =
    { table : Maybe Table.Model
    , error : Maybe Http.Error
    }


type alias UserRecord =
    { id : Int
    , isActive : Bool
    , balance : Float
    , age : Int
    , name : String
    }


type Msg
    = NoOp
    | TableMsg Table.Msg
    | FetchFail Http.Error
    | FetchSucceed (List UserRecord)
    | LoadPeople


loadPeople : Cmd Msg
loadPeople =
    let
        url =
            "/res/people.json"
    in
        Task.perform FetchFail FetchSucceed (Http.get decodePeople url)


decodePeople : Decoder (List UserRecord)
decodePeople =
    let
        decoder =
            succeed UserRecord
                |: ("_id" := int)
                |: ("isActive" := bool)
                |: ("balance" := Json.float)
                |: ("age" := int)
                |: ("name" := Json.string)
    in
        at [ "data" ] (list decoder)


headers : List Header.Model
headers =
    [ { title = "Id", state = Original, type' = IntType }
    , { title = "Active", state = Original, type' = IntType }
    , { title = "Balance", state = Original, type' = StringType }
    , { title = "Age", state = Original, type' = DateType }
    , { title = "Name", state = Original, type' = FloatType }
    ]


getRecords : List UserRecord -> List (Record.Model)
getRecords list =
    List.map (toRecord) list


toRecord : UserRecord -> Record.Model
toRecord rec =
    Record.init
        [ Cell.init (I rec.id) True
        , Cell.init (B rec.isActive) True
        , Cell.init (F rec.balance) True
        , Cell.init (D rec.age) True
        , Cell.init (S rec.name) True
        ]


init : ( Model, Cmd Msg )
init =
    ( Model Nothing Nothing, loadPeople )


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "row" ]
            [ case model.table of
                Nothing ->
                    div [] [ text "Select a customer" ]

                Just table ->
                    (App.map TableMsg (Table.view table))
            ]
        , case model.error of
            Nothing ->
                div [] []

            Just e ->
                text (Basics.toString e)
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        TableMsg msg ->
            case model.table of
                Nothing ->
                    ( model, Cmd.none )

                Just table ->
                    let
                        ( newTable, cmds ) =
                            Table.update msg table
                    in
                        ( { model | table = Just newTable }, Cmd.map TableMsg cmds )

        FetchFail error ->
            ( { model | error = Just error, table = Nothing }, Cmd.none )

        FetchSucceed rec ->
            let
                records =
                    getRecords rec

                table =
                    Table.init ("Example") headers records True
            in
                ( { model | table = Just table }, Cmd.none )

        LoadPeople ->
            ( model, loadPeople )


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        tagger code =
            if code == 13 then
                msg
            else
                NoOp
    in
        on "keydown" (Json.map tagger keyCode)


main : Program Never
main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }
