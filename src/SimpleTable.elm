module SimpleTable exposing (..)

{-|

-}

import ExternalCSS exposing (..)
import Header
import Html exposing (..)
import Html.App as App exposing (program)
import Html.Attributes exposing (class)
import Array exposing (..)


--MODEL


type alias IdHeader =
    { id : Int
    , model : Header.Model
    }


type alias Model =
    { headers : List IdHeader
    }



--INIT


init : List String -> ( Model, Cmd Msg )
init list =
    let
        array =
            fromList list

        indexedList =
            toIndexedList array
    in
        ( Model (List.map initHelper indexedList), Cmd.none )


initHelper : ( Int, String ) -> IdHeader
initHelper ( i, value ) =
    IdHeader i (Header.initNoCmd value)



--UPDATE


type Msg
    = NoOp
    | HeaderMsg Int Header.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        HeaderMsg id msg ->
            let
                ( newHeaders, cmds ) =
                    List.unzip (List.map (updateHelp id msg) model.headers)
            in
                ( { model | headers = newHeaders }, Cmd.batch cmds )


updateHelp : Int -> Header.Msg -> IdHeader -> ( IdHeader, Cmd Msg )
updateHelp id msg header =
    if header.id /= id then
        ( header, Cmd.none )
    else
        let
            ( newModel, cmds ) =
                Header.update msg header.model
        in
            ( IdHeader id newModel, Cmd.map (HeaderMsg id) cmds )


view : Model -> Html Msg
view model =
    div []
        [ (stylesheet)
        , table [ class "table" ]
            [ thead []
                [ tr [] (List.map viewHeader model.headers)
                ]
            ]
        ]


viewHeader : IdHeader -> Html Msg
viewHeader { id, model } =
    App.map (HeaderMsg id) (Header.view model)


main : Program Never
main =
    program
        { init = init [ "Name", "Surname", "DofBirth", "Test" ]
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }
