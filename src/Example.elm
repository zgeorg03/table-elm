module Main exposing (..)

import Table exposing (..)
import Html exposing (..)
import Html.App as App exposing (program)


type alias Model =
    { table : Maybe Table.Model
    }


type Msg
    = NoOp


headers : List Table.Header.Model
headers =
    [ { title = "ID", state = Original, type' = IntType }
    , { title = "Name", state = Original, type' = StringType }
    , { title = "Surname", state = Original, type' = StringType }
    , { title = "DateOfBirth", state = Original, type' = DateType }
    , { title = "Rich", state = Original, type' = BoolType }
    ]


init : ( Model, Cmd Msg )
init =
    ( Model Nothing, Cmd.none )


view : Model -> Html Msg
view model =
    div [] [ text "test" ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


main : Program Never
main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }
