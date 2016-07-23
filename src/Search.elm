module Search exposing (Model, Msg, init, update, view)

{-| Search module

@docs Model, Msg, init, update, view
-}

import Basics
import Html exposing (..)
import Html.App as App exposing (program)
import Html.Attributes exposing (class, placeholder)
import Html.Events exposing (onInput, on, keyCode)
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
    }


{-| Actions
-}
type Msg
    = NoOp
    | ChangeValue String
    | UpdateSearch


{-| Init method
-}
init : List String -> Model
init list =
    let
        indexedList =
            Array.fromList list |> Array.toIndexedList
    in
        Model "" indexedList (Array.initialize (List.length list) (\n -> n) |> Array.toList)


{-| Init with command method
-}
initCmd : List String -> ( Model, Cmd Msg )
initCmd list =
    ( init list, Cmd.none )


{-| Update method
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ChangeValue str ->
            ( { model | value = str }, Cmd.none )

        UpdateSearch ->
            let
                res =
                    SafeRegex.safeRegex model.value

                regex =
                    case res of
                        Err _ ->
                            SafeRegex.regex ""

                        Ok a ->
                            a

                ( searchList, _ ) =
                    List.unzip (filterList regex model.data)
            in
                ( { model | searchList = searchList }, Cmd.none )


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
    input [ onInput ChangeValue, onEnter UpdateSearch, placeholder "Search" ] []


main : Program Never
main =
    program
        { view = view
        , init = initCmd [ "123124 Zacharias Georgiou 18 Dec", "9534 Testing Marios 03 March" ]
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }
