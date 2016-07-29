module Record exposing (Msg, Model, init, update, view, toString, toCsv)

{-| This module represents a list of Cells

@docs Msg, Model, init, update, view, toString, toCsv
-}

import Value exposing (..)
import Cell exposing (..)
import Html exposing (..)
import Array exposing (..)
import Html.App as App exposing (program)
import String


type alias IdCell =
    { id : Int
    , model : Cell.Model
    }


{-| The model is a list of unique cells
-}
type alias Model =
    Array IdCell


{-| Initialization of the model. It needs a list of cells
-}
init : List Cell.Model -> Model
init list =
    let
        array =
            fromList list

        indexedList =
            toIndexedList array
    in
        List.map initHelper indexedList |> Array.fromList


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
        ( List.map initHelper indexedList |> Array.fromList
        , Cmd.none
        )


{-| Helper method in order to extend a cell into a cell with id
-}
initHelper : ( Int, Cell.Model ) -> IdCell
initHelper ( i, model ) =
    IdCell i model


{-| Possible actions
-}
type Msg
    = CellMsg Int Cell.Msg


{-| To csv method
-}
toCsv : Model -> String
toCsv model =
    let
        array =
            Array.map (\x -> x.model) model
    in
        Array.map Cell.toCsv array |> Array.toList |> String.join ","


{-| To String method. Each cell is seperated by #{column number}#
-}
toString : Model -> String
toString model =
    let
        array =
            Array.map (\x -> x.model) model
    in
        "#" ++ (Array.map Cell.toString array |> addHashCol |> String.join "#")


{-| Helper function to add the number in each column
-}
addHashCol : Array String -> List String
addHashCol array =
    Array.toIndexedList array |> List.map mapPair


{-| Add the number of the column
-}
mapPair : ( Int, String ) -> String
mapPair ( id, value ) =
    (Basics.toString (id + 1)) ++ "#" ++ value


{-| Update the model based on the appropriate action
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CellMsg id msg ->
            let
                ( newCells, cmds ) =
                    Array.map (updateHelp id msg) model |> Array.toList |> List.unzip
            in
                ( Array.fromList newCells, Cmd.batch cmds )


{-| Helper function to update the correct cell
-}
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
    tr [] (Array.map viewCell model |> Array.toList)


{-| View of the cell
-}
viewCell : IdCell -> Html Msg
viewCell { id, model } =
    td [] [ App.map (CellMsg id) (Cell.view model) ]


{-| Example model
-}
model : List Cell.Model
model =
    [ Cell.init (I 0) True
    , Cell.init (B True) True
    , Cell.init (F 0) True
    , Cell.init (D 12312312) True
    ]


{-| Visually check this module
-}
main : Program Never
main =
    program
        { init = initWithCmd model
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }
