module NewRecord exposing (..)

import Value exposing (..)
import Cell exposing (..)
import Html exposing (..)
import Html.App as App exposing (program)
import Html.Attributes exposing (value, type', checked)
import Array exposing (..)


--MODEL


type alias Model =
    { records : Array Cell.Model
    }



--UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


emptyRecord : List Value -> Array Value
emptyRecord list =
    let
        array =
            Array.fromList list
    in
        Array.map Value.getDefaultValue array



--INIT


init : List ValueType -> ( Model, Cmd Msg )
init list =
    ( Model (emptyRecord list), Cmd.none )



--rerIEW


view : Model -> Html Msg
view model =
    div []
        [ createRecords model
        ]


createRecords : Model -> Html Msg
createRecords model =
    tr [] (Array.toList (Array.map createRecord model.records))


createRecord : Value -> Html Msg
createRecord val =
    case val of
        I int ->
            input [ type' "text", value (Basics.toString int) ] []

        F float ->
            input [ type' "text", value (Basics.toString float) ] []

        B bool ->
            input [ type' "checkbox", checked bool ] []

        D date ->
            input [ type' "date", value (Value.toString val) ] []

        S str ->
            input [ type' "text", value str ] []


main : Program Never
main =
    program
        { view = view
        , update = update
        , init = init [ StringType, IntType, BoolType, FloatType, DateType ]
        , subscriptions = (\_ -> Sub.none)
        }
