module Cell exposing (Model, Msg, init, initEditable, update, view, toString)

{-| This module implements an input field with validation

@docs Model, Msg, init, initEditable, update, view, toString

-}

import Value exposing (..)
import Html exposing (..)
import Html.Attributes exposing (style, value, type', readonly, checked, disabled)
import Html.App exposing (program)
import Html.Events exposing (onClick, keyCode, on, onInput)
import String
import Json.Decode as Json


--Model


{-| Model Description
-}
type alias Model =
    { value : Value
    , visible : Bool
    , rawValue : String
    , readonly : Bool
    }


{-| Initialization of a model without command
-}
init : Value -> Bool -> Model
init value visible =
    Model value visible (Value.toString value) True


{-| Initialization of a model without command in edit mode
-}
initEditable : Value -> Bool -> Model
initEditable value visible =
    Model value visible (Value.toString value) False


{-| Initialization of a model
-}
initCmd : Value -> Bool -> Bool -> ( Model, Cmd Msg )
initCmd val visible readonly =
    ( Model val visible (Value.toString val) readonly
    , Cmd.none
    )



--Update


{-| All the possible actions a cell can perfrom
-}
type Msg
    = Pass
    | Setreadonly
    | Unsetreadonly
    | UpdateRawInput String
    | UpdateString
    | UpdateInteger
    | UpdateFloat
    | UpdateBool


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

        Setreadonly ->
            ( { model | readonly = True }, Cmd.none )

        Unsetreadonly ->
            ( { model | readonly = False }, Cmd.none )

        UpdateRawInput str ->
            ( { model | rawValue = str }, Cmd.none )

        UpdateString ->
            ( { model | value = S model.rawValue }, Cmd.none )

        UpdateInteger ->
            let
                ( val, rawVal ) =
                    case String.toInt model.rawValue of
                        Ok value ->
                            ( I value, Basics.toString value )

                        Err error ->
                            ( Value.getDefaultValue model.value, "" )
            in
                ( { model | rawValue = rawVal, value = val }, Cmd.none )

        UpdateFloat ->
            let
                ( val, rawVal ) =
                    case String.toFloat model.rawValue of
                        Ok value ->
                            ( F value, Basics.toString value )

                        Err error ->
                            ( Value.getDefaultValue model.value, "" )
            in
                ( { model | rawValue = rawVal, value = val }, Cmd.none )

        UpdateBool ->
            case model.readonly of
                True ->
                    ( model, Cmd.none )

                False ->
                    let
                        val : Value
                        val =
                            case model.value of
                                B b ->
                                    B (not b)

                                _ ->
                                    model.value
                    in
                        ( { model | value = val }, Cmd.none )


{-| The view of the model
-}
view : Model -> Html Msg
view model =
    case model.visible of
        True ->
            case model.readonly of
                False ->
                    case model.value of
                        I int ->
                            input [ onEnter UpdateInteger, onInput UpdateRawInput, type' "text", value model.rawValue ] []

                        F float ->
                            input [ onEnter UpdateFloat, onInput UpdateRawInput, type' "text", value model.rawValue ] []

                        B bool ->
                            input [ onClick UpdateBool, type' "checkbox", checked bool ] []

                        D date ->
                            input [ readonly True, type' "date", value (Value.toString model.value) ] []

                        S str ->
                            input [ onEnter UpdateString, onInput UpdateRawInput, type' "text", value model.rawValue ] []

                        E ->
                            input [ readonly True, type' "text", value (Value.toString model.value) ] []

                True ->
                    case model.value of
                        B bool ->
                            input [ disabled True, readonly model.readonly, type' "checkbox", checked bool ] []

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
        { init = initCmd (F 1213) False False
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }
