module Header exposing (State(..), Model, Msg, init, update, view, reset, toCsv)

{-| This module builds a simple Header, with 3 states: Original,Ascending, Descending

@docs State, Model, Msg, init, update, view, reset, toCsv
-}

import Value exposing (..)
import Html exposing (..)
import Html.App exposing (program)
import Html.Events exposing (onClick, onDoubleClick)
import Html.Attributes exposing (class)


{-| All the possible states a header can be
-}
type State
    = Original
    | Ascending
    | Descending


{-| A model has a value and a state
-}
type alias Model =
    { title : String
    , state : State
    , type' : ValueType
    }


{-| Initialization of the model
-}
initCmd : String -> ValueType -> ( Model, Cmd Msg )
initCmd title typ =
    ( init title typ, Cmd.none )


{-| Reset the model to the original state
-}
reset : Model -> Model
reset header =
    Model header.title Original header.type'


{-| Initialization of the model without command
-}
init : String -> ValueType -> Model
init title typ =
    Model title Original typ


{-| To Csv
-}
toCsv : Model -> String
toCsv model =
    "\"" ++ model.title ++ "\""


{-| All the possible actions
-}
type Msg
    = Reset
    | ChangeState


{-| Update the model
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            ( { model | state = Original }, Cmd.none )

        ChangeState ->
            case model.state of
                Original ->
                    ( { model | state = Ascending }, Cmd.none )

                Ascending ->
                    ( { model | state = Descending }, Cmd.none )

                Descending ->
                    ( { model | state = Original }, Cmd.none )


{-| The view of the model
-}
view : Model -> Html Msg
view model =
    showHeader model


{-| Show header
-}
showHeader : Model -> Html Msg
showHeader model =
    case model.state of
        Original ->
            th []
                [ label [ onClick ChangeState, onDoubleClick Reset ]
                    [ text (model.title ++ " ")
                    , span [ class "glyphicon glyphicon-minus" ] []
                    ]
                ]

        Ascending ->
            th []
                [ label [ onClick ChangeState, onDoubleClick Reset ]
                    [ text model.title
                    , span [ class "glyphicon glyphicon-triangle-bottom" ] []
                    ]
                ]

        Descending ->
            th []
                [ label [ onClick ChangeState, onDoubleClick Reset ]
                    [ text model.title
                    , span [ class "glyphicon glyphicon-triangle-top" ] []
                    ]
                ]


{-| Visually check this module
-}
main : Program Never
main =
    program
        { init = initCmd "Name" IntType
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
