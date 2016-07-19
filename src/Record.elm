module Record exposing (Msg, Model, init, update, view)

{-| This module represents a list of Cells

@docs Msg, Model, init, update, view
-}

import Value exposing (..)
import Cell exposing (..)
import Html exposing (..)
import Array exposing (..)
import Html.App as App exposing (program)


type alias IdCell =
    { id : Int
    , model : Cell.Model
    }


{-| The model is a list of unique cells
-}
type alias Model =
    List IdCell



--Init


{-| Initialization of the model
-}
init : List Cell.Model -> Model
init list =
    let
        array =
            fromList list

        indexedList =
            toIndexedList array
    in
        List.map initHelper indexedList


{-| Initialization of the model with Cmds
-}
initWithCmd : List Cell.Model -> ( Model, Cmd Msg )
initWithCmd list =
    let
        array =
            fromList list

        indexedList =
            toIndexedList array
    in
        ( List.map initHelper indexedList
        , Cmd.none
        )


initHelper : ( Int, Cell.Model ) -> IdCell
initHelper ( i, model ) =
    IdCell i model



--Update


{-| Possible actions
-}
type Msg
    = CellMsg Int Cell.Msg


{-| Update the model based on the appropriate action
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CellMsg id msg ->
            let
                ( newCells, cmds ) =
                    List.unzip (List.map (updateHelp id msg) model)
            in
                ( newCells, Cmd.batch cmds )


updateHelp : Int -> Cell.Msg -> IdCell -> ( IdCell, Cmd Msg )
updateHelp id msg idcell =
    if idcell.id /= id then
        ( idcell, Cmd.none )
    else
        let
            ( newModel, cmds ) =
                Cell.update msg idcell.model
        in
            ( IdCell id newModel, Cmd.map (CellMsg id) cmds )


{-| The view of the model
-}
view : Model -> Html Msg
view model =
    tr [] (List.map viewCell model)


viewCell : IdCell -> Html Msg
viewCell { id, model } =
    td [] [ App.map (CellMsg id) (Cell.view model) ]


model : List Cell.Model
model =
    [ Cell.init (I 0)
    , Cell.init (B True)
    , Cell.init (F 0)
    , Cell.init (D 12312312)
    ]


main : Program Never
main =
    program
        { init = initWithCmd model
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }