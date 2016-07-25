module Search exposing (Model, Msg, init, update, view)

{-| Search module

@docs Model, Msg, init, update, view
-}

import Basics
import Html exposing (..)
import Html.App as App exposing (program)
import Html.Attributes exposing (class, placeholder, style, value, type', checked)
import Html.Events exposing (onInput, on, keyCode, onClick)
import Json.Decode as Json


--import String

import Array
import SafeRegex exposing (..)


{-| Model definition
-}
type alias Model =
    { value : String
    , data : List ( Int, String )
    , searchList : List Int
    , automated : Bool
    , caseInsensitive : Bool
    }


{-| Actions
-}
type Msg
    = NoOp
    | ChangeValue String
    | UpdateSearch
    | ClearSearch
    | UpdateCaseSensitivity


{-| Init method
-}
init : Bool -> List String -> Model
init auto list =
    let
        indexedList =
            Array.fromList list |> Array.toIndexedList
    in
        Model "" indexedList (Array.initialize (List.length list) (\n -> n) |> Array.toList) auto False


{-| Init with command method
-}
initCmd : Bool -> List String -> ( Model, Cmd Msg )
initCmd auto list =
    ( init auto list, Cmd.none )


{-| Update method
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UpdateCaseSensitivity ->
            let
                old =
                    model.caseInsensitive
            in
                ( { model | caseInsensitive = not old, searchList = (Array.initialize (List.length model.data) (\n -> n) |> Array.toList), value = "" }, Cmd.none )

        ChangeValue str ->
            case model.automated of
                False ->
                    ( { model | value = str }, Cmd.none )

                True ->
                    case model.caseInsensitive of
                        False ->
                            case SafeRegex.safeRegex str of
                                Ok regex ->
                                    let
                                        ( searchList, _ ) =
                                            List.unzip (filterList regex model.data)
                                    in
                                        ( { model | value = str, searchList = searchList }, Cmd.none )

                                Err _ ->
                                    ( { model | value = str, searchList = (Array.initialize (List.length model.data) (\n -> n) |> Array.toList) }, Cmd.none )

                        True ->
                            case SafeRegex.safeRegexInsensitive str of
                                Ok regex ->
                                    let
                                        ( searchList, _ ) =
                                            List.unzip (filterList regex model.data)
                                    in
                                        ( { model | value = str, searchList = searchList }, Cmd.none )

                                Err _ ->
                                    ( { model | value = str, searchList = (Array.initialize (List.length model.data) (\n -> n) |> Array.toList) }, Cmd.none )

        ClearSearch ->
            ( { model | searchList = (Array.initialize (List.length model.data) (\n -> n) |> Array.toList), value = "" }, Cmd.none )

        UpdateSearch ->
            case SafeRegex.safeRegex model.value of
                Ok regex ->
                    let
                        ( searchList, _ ) =
                            List.unzip (filterList regex model.data)
                    in
                        ( { model | searchList = searchList }, Cmd.none )

                Err _ ->
                    ( { model | searchList = (Array.initialize (List.length model.data) (\n -> n) |> Array.toList), value = "" }, Cmd.none )


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


filterList : Regex -> List ( Int, String ) -> List ( Int, String )
filterList search list =
    List.filter (findString search) list


findString : Regex -> ( Int, String ) -> Bool
findString search ( _, text ) =
    SafeRegex.contains search text


{-| View Method
-}
view : Model -> Html Msg
view model =
    div []
        [ div [ class "row" ]
            [ div [ class "col-md-4" ]
                [ input [ style [ ( "width", "80%" ) ], onInput ChangeValue, onEnter UpdateSearch, placeholder "Search", value model.value ] []
                , button [ onClick ClearSearch ]
                    [ span [ class "glyphicon glyphicon-remove" ] []
                    ]
                ]
            ]
        , div [ class "row" ]
            [ div [ class "col-md-4" ]
                [ text "Case Insensitive   "
                , input [ onClick UpdateCaseSensitivity, type' "checkbox", checked model.caseInsensitive ] []
                ]
            ]
        ]


main : Program Never
main =
    program
        { view = view
        , init = initCmd True [ "123124 Zacharias Georgiou 18 Dec", "9534 Testing Marios 03 March" ]
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }
