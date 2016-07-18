module Cell exposing (..)

import Value exposing (..)
import Html exposing (..)
import Html.Attributes exposing (style, value, type', readonly, checked)
import Html.App exposing (program)
import Html.Events exposing (onDoubleClick, keyCode, on, onInput)
import Json.Decode as Json


--Model


type alias Model =
    { value : Value
    , readonly : Bool
    }


init : Value -> ( Model, Cmd Msg )
init val =
    ( Model val True
    , Cmd.none
    )



--Update


type Msg
    = Pass
    | UpdateValue String
    | Setreadonly
    | Unsetreadonly


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Pass ->
            ( model, Cmd.none )

        Setreadonly ->
            ( { model | readonly = True }, Cmd.none )

        Unsetreadonly ->
            ( { model | readonly = False }, Cmd.none )

        UpdateValue v ->
            {- let
                   p =
                       S v
               in
                   ( { model | data = v }, Cmd.none )

            -}
            ( { model | readonly = False }, Cmd.none )



--View


view : Model -> Html Msg
view model =
    div []
        [ case model.value of
            I int ->
                input [ readonly model.readonly, type' "text", value (Basics.toString int) ] []

            F float ->
                input [ readonly model.readonly, type' "text", value (Basics.toString float) ] []

            B bool ->
                input [ readonly model.readonly, type' "checkbox", checked bool ] []

            D date ->
                input [ readonly model.readonly, type' "date", value (Value.toString model.value) ] []

            S str ->
                input [ readonly model.readonly, type' "text", value str ] []
        ]


onEnter : Msg -> Attribute Msg
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
    program
        { init = init (S "Zack")
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }
