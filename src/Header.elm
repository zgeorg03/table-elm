module Header exposing (State(..), Model, Msg, init, initNoCmd, update, view)

{-| This module builds a simple Header, with 3 states: Original,Ascending, Descending

@docs State, Model, Msg, init, initNoCmd, update, view
-}

import Value exposing (..)
import Html exposing (..)
import Html.App exposing (program)
import Html.Events exposing (onClick, onDoubleClick)
import Html.Attributes exposing (class)


--MODEL


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



--INIT


{-| Initialization of the model
-}
init : String -> ValueType -> ( Model, Cmd Msg )
init title typ =
    ( Model title Original typ, Cmd.none )


{-| Initialization of the model without command
-}
initNoCmd : String -> ValueType -> Model
initNoCmd title typ =
    Model title Original typ



--UPDATE


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



--VIEW


{-| The view of the model
-}
view : Model -> Html Msg
view model =
    showHeader model


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


main : Program Never
main =
    program
        { init = init "Name" IntType
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
