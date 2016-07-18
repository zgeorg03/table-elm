module SimpleTable exposing (..)

{-|

-}

import ExternalCSS exposing (..)
import Header exposing (..)
import Value exposing (..)
import Html exposing (..)
import Html.App as App exposing (program)
import Html.Attributes exposing (class, value)
import Html.Events exposing (onInput, onClick)
import Array exposing (..)


--MODEL


type alias IdHeader =
    { id : Int
    , model : Header.Model
    }


type alias Model =
    { headers : List IdHeader
    , data : List (Array Value)
    , size : Int
    , error : Maybe String
    , newRecord : Array Value
    }



--INIT


init : List Header.Model -> ( Model, Cmd Msg )
init list =
    let
        array =
            fromList list

        len =
            length array

        indexedList : List ( Int, Header.Model )
        indexedList =
            toIndexedList array
    in
        ( Model (List.map initHelper indexedList) [] len Nothing empty
        , Cmd.none
        )


initHelper : ( Int, Header.Model ) -> IdHeader
initHelper ( i, model ) =
    IdHeader i (model)



--UPDATE


type Msg
    = NoOp
    | HeaderMsg Int Header.Msg
    | UpdateCol1 String
    | AddRecord


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

        UpdateCol1 txt ->
            let
                len =
                    length model.newRecord

                array =
                    if len /= model.size then
                        emptyRecord model
                    else
                        model.newRecord

                newArray =
                    set 0 (makeString txt) array
            in
                ( { model | newRecord = newArray }, Cmd.none )

        AddRecord ->
            let
                res =
                    addRecord model.newRecord model
            in
                case res of
                    Err error ->
                        ( { model | error = Just error }, Cmd.none )

                    Ok newModel ->
                        ( { newModel | newRecord = (emptyRecord newModel) }, Cmd.none )


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



--Add record


addRecord : Array Value -> Model -> Result String Model
addRecord array model =
    let
        len =
            (length array)

        newData =
            array :: model.data

        res =
            if len /= model.size then
                Err "Record size doesn't match with the table definition"
            else
                Ok { model | data = newData }
    in
        res


emptyRecord : Model -> Array Value
emptyRecord model =
    initialize model.size (always (S ""))


view : Model -> Html Msg
view model =
    let
        txtCol1 =
            case (get 0 model.newRecord) of
                Just val ->
                    (Value.toString val)

                Nothing ->
                    ""
    in
        div []
            [ div []
                [ (stylesheet)
                , table [ class "table" ]
                    [ thead []
                        [ tr [] (List.map viewHeader model.headers)
                        ]
                    , case model.error of
                        Nothing ->
                            tbody []
                                (viewRecords model.data)

                        Just error ->
                            tr [] [ text error ]
                    ]
                ]
            , div []
                [ input [ onInput UpdateCol1, value txtCol1 ] []
                , button [ onClick AddRecord ] [ text "Add" ]
                ]
            ]


viewRecords : List (Array Value) -> List (Html Msg)
viewRecords list =
    List.map viewRecord list


viewRecord : Array Value -> Html Msg
viewRecord array =
    tr [] (toList (Array.map viewRecordCell array))


viewRecordCell : Value -> Html Msg
viewRecordCell value =
    td [] [ text (Value.toString value) ]


viewHeader : IdHeader -> Html Msg
viewHeader { id, model } =
    App.map (HeaderMsg id) (Header.view model)


model : List Header.Model
model =
    [ { title = "ID", state = Original, type' = IntType }
    , { title = "Name", state = Original, type' = StringType }
    , { title = "Surname", state = Original, type' = StringType }
    , { title = "DateOfBirth", state = Original, type' = DateType }
    ]


main : Program Never
main =
    program
        { init = init model
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }
