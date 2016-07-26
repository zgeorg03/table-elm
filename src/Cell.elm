module Cell exposing (Model, Msg, init, update, view, toString, toCsv)

{-| This module implements an input field with validation

# Basics
@docs Model, Msg, init,  update, view, toString, toCsv

-}

import Value exposing (..)
import Html exposing (..)
import Html.Attributes exposing (style, value, type', readonly, checked, disabled)
import Html.App exposing (program)
import Html.Events exposing (onClick, keyCode, on, onInput)
import Json.Decode as Json


--Model


{-| Model Description
-}
type alias Model =
    { value : Value
    , visible : Bool
    }


{-| Initialization of a model without command
-}
init : Value -> Bool -> Model
init value visible =
    Model value visible


{-| Initialization of a model
-}
initCmd : Value -> Bool -> ( Model, Cmd Msg )
initCmd value visible =
    ( init value visible, Cmd.none )



--Update


{-| All the possible actions a cell can perfrom
-}
type Msg
    = Pass


{-| To csv method
-}
toCsv : Model -> String
toCsv model =
    if model.visible then
        "\"" ++ Value.toString model.value ++ "\""
    else
        "\"\""


{-| To string method
-}
toString : Model -> String
toString model =
    Value.toString model.value


{-| Update the model
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Pass ->
            ( model, Cmd.none )


{-| The view of the model
-}
view : Model -> Html Msg
view model =
    case model.visible of
        True ->
            case model.value of
                B bool ->
                    input [ disabled True, type' "checkbox", checked bool ] []

                _ ->
                    text (Value.toString model.value)

        False ->
            text "          "


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
        { init = initCmd (F 1213) True
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }
