module Cell exposing (..)

import Value exposing (..)
import Html exposing (..)
import Html.Attributes exposing (style, value)
import Html.App exposing (beginnerProgram)
import Html.Events exposing (onDoubleClick, keyCode, on)
import Json.Decode as Json


--Model


type alias Model a =
    { data : Value a
    , editable : Bool
    , backgroundColor : String
    , editingValue : String
    }


model : Model S
model =
    Model (makeString "Hello, This is a test") False "" ""



--Update


type Msg a
    = Pass
    | UpdateValue String
    | SetEditable
    | UnsetEditable


update : Msg a -> Model a -> Model a
update msg model =
    case msg of
        Pass ->
            model

        SetEditable ->
            { model | editable = True }

        UnsetEditable ->
            { model | editable = False }

        UpdateValue v ->
            { model | data = v }



--View


view : Model a -> Html (Msg a)
view model =
    case model.editable of
        False ->
            td [ onDoubleClick SetEditable, style [ ( "background-color", model.backgroundColor ) ] ] [ text (Value.toString model.data) ]

        True ->
            td [ onDoubleClick UnsetEditable, onEnter UpdateValue, style [ ( "background-color", model.backgroundColor ) ] ]
                [ input [ value (Value.toString model.data) ] []
                ]


onEnter : Msg a -> Attribute (Msg a)
onEnter msg =
    let
        tagger code =
            if code == 13 then
                msg
            else
                Pass
    in
        on "keydown" (Json.map tagger keyCode)



--Main


main : Program Never
main =
    beginnerProgram { model = model, view = view, update = update }
