module TableExample exposing (..)

import Table exposing (..)
import Record exposing (..)
import Cell exposing (..)
import Value exposing (..)
import Header exposing (..)
import Http exposing (..)
import Html exposing (..)
import Html.App as App exposing (program)
import Html.Attributes as Attributes exposing (placeholder, class, style, type', value)
import Html.Events exposing (onClick, onInput, on, keyCode)
import Task
import Json.Decode as Json exposing (..)
import Json.Decode.Extra exposing (..)


type alias Model =
    { table : Maybe Table.Model
    , error : Maybe Http.Error
    , url : String
    }


type alias UserRecord =
    { id : Int
    , name : String
    , surname : String
    , isActive : Bool
    , balance : Float
    , age : Int
    , email : String
    }


type Msg
    = NoOp
    | TableMsg Table.Msg
    | FetchFail Http.Error
    | FetchSucceed (List UserRecord)
    | LoadPeople
    | UpdateInput String


loadPeople : String -> Cmd Msg
loadPeople url =
    Task.perform FetchFail FetchSucceed (Http.get decodePeople url)


decodePeople : Decoder (List UserRecord)
decodePeople =
    let
        decoder =
            succeed UserRecord
                |: ("_id" := int)
                |: ("name" := Json.string)
                |: ("surname" := Json.string)
                |: ("isActive" := bool)
                |: ("balance" := Json.float)
                |: ("age" := int)
                |: ("email" := Json.string)
    in
        at [ "data" ] (list decoder)


headers : List Header.Model
headers =
    [ { title = "Id", state = Original, type' = IntType }
    , { title = "Name", state = Original, type' = StringType }
    , { title = "Surname", state = Original, type' = StringType }
    , { title = "Balance", state = Original, type' = FloatType }
    , { title = "Age", state = Original, type' = DateType }
    , { title = "Email", state = Original, type' = StringType }
    , { title = "Active", state = Original, type' = BoolType }
    ]


getRecords : List UserRecord -> List (Record.Model)
getRecords list =
    List.map (toRecord) list


toRecord : UserRecord -> Record.Model
toRecord rec =
    Record.init
        [ Cell.init (I rec.id) True
        , Cell.init (S rec.name) True
        , Cell.init (S rec.surname) True
        , Cell.init (F rec.balance) True
        , Cell.init (D rec.age) True
        , Cell.init (S rec.email) True
        , Cell.init (B rec.isActive) True
        ]


init : String -> ( Model, Cmd Msg )
init url =
    ( Model Nothing Nothing url, loadPeople url )


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div []
            [ div [ class "form-group" ]
                [ label [] [ text "Fetch data from:   " ]
                , input [ onEnter LoadPeople, onInput UpdateInput, placeholder "Url", type' "text", Attributes.value model.url ] []
                ]
            ]
        , div [ class "row" ]
            [ case model.table of
                Nothing ->
                    div [] []

                Just table ->
                    (App.map TableMsg (Table.view table))
            ]
        , case model.error of
            Nothing ->
                div [] []

            Just e ->
                div
                    [ style [ ( "color", "red" ) ] ]
                    [ text
                        (Basics.toString e)
                    ]
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
            ( { model | error = Nothing }, loadPeople model.url )

        UpdateInput str ->
            ( { model | url = str }, Cmd.none )


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
        { init = init "/res/people.json"
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }
