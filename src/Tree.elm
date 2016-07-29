module Tree exposing (..)

import ExternalCSS exposing (..)
import Html exposing (..)
import Html.App as App exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (type', checked, class, style)


type alias Node =
    { name : String
    , childrenVisible : Bool
    , children : Children
    , path : List Int
    }


type alias Model =
    { root : Node
    , currentNode : Node
    }


type Children
    = C (List Node)


type Msg
    = Toggle (List Int)
    | UpdateCurrentNode (List Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Toggle path ->
            let
                newModel =
                    updateTree msg path model.root

                currentModel =
                    findModel path model.root
            in
                ( { model | root = newModel, currentNode = currentModel }, Cmd.none )

        UpdateCurrentNode path ->
            let
                currentModel =
                    findModel path model.root
            in
                ( { model | currentNode = currentModel }, Cmd.none )


findModel : List Int -> Node -> Node
findModel path n =
    case path of
        [] ->
            n

        x :: rest ->
            let
                children =
                    case n.children of
                        C list ->
                            list

                mayChild =
                    List.drop x children |> List.head

                child =
                    case mayChild of
                        Nothing ->
                            Debug.crash ("Should not happen")

                        Just a ->
                            a
            in
                findModel rest child


toggleVisible : Msg -> Node -> Node
toggleVisible msg model =
    case msg of
        Toggle path ->
            { model | childrenVisible = not model.childrenVisible }

        UpdateCurrentNode path ->
            model


updateTree : Msg -> List Int -> Node -> Node
updateTree msg path model =
    case path of
        --Root case
        [] ->
            toggleVisible msg model

        x :: rest ->
            let
                list =
                    case model.children of
                        C c ->
                            c

                c =
                    List.indexedMap (updateChildren msg x rest) list
            in
                { model | children = C c }


updateChildren : Msg -> Int -> List Int -> Int -> Node -> Node
updateChildren msg id path index model =
    if (id /= index) then
        model
    else
        updateTree msg path model


constructPath : ( List Int, Node ) -> Node
constructPath ( path, model ) =
    case isLeaf model of
        True ->
            { model | path = List.reverse path }

        False ->
            let
                children =
                    case model.children of
                        C c ->
                            c

                imm =
                    List.indexedMap (appendPath path) children

                x =
                    imm |> List.map constructPath
            in
                { model | path = List.reverse path, children = C x }


appendPath : List Int -> Int -> Node -> ( List Int, Node )
appendPath path index model =
    ( index :: path, model )


isLeaf : Node -> Bool
isLeaf model =
    case model.children of
        C list ->
            List.isEmpty list


view : Model -> Html Msg
view model =
    div []
        [ (stylesheet)
        , div [ class "panel panel-default" ]
            [ div [ class "panel-heading" ] [ text "Navigation" ]
            , div [ class "panel-body" ] [ viewTree model model.root ]
            ]
        ]


viewTree : Model -> Node -> Html Msg
viewTree tree model =
    case isLeaf model of
        True ->
            h4 [ selected tree model, onClick (UpdateCurrentNode model.path) ]
                [ span [ class "glyphicon glyphicon-minus" ] []
                , text (model.name)
                ]

        False ->
            case model.childrenVisible of
                False ->
                    h4 [ selected tree model, onClick (Toggle model.path) ]
                        [ span [ style [ ( "color", "blue" ) ], class "glyphicon glyphicon-plus" ] []
                        , text (model.name)
                        ]

                True ->
                    div []
                        [ h4 [ selected tree model, onClick (Toggle model.path) ]
                            [ span [ style [ ( "color", "red" ) ], class "glyphicon glyphicon-minus" ] []
                            , text (model.name)
                            ]
                        , ul [ class "" ] (viewChildren tree model.children)
                        ]


selected : Model -> Node -> Attribute Msg
selected tree n2 =
    let
        curr =
            tree.currentNode
    in
        if (containedInPath curr.path n2.path) then
            style [ ( "background-color", "#eee" ) ]
        else
            style [ ( "background-color", "#fff" ) ]


containedInPath : List Int -> List Int -> Bool
containedInPath path list =
    case path of
        [] ->
            case list of
                [] ->
                    True

                x :: r ->
                    False

        p :: restPath ->
            case list of
                [] ->
                    True

                x :: restList ->
                    if (p == x) then
                        containedInPath restPath restList
                    else
                        False


viewChildren : Model -> Children -> List (Html Msg)
viewChildren tree children =
    case children of
        C list ->
            List.map (viewTree tree) list


createLinux : Node
createLinux =
    let
        x =
            Node "/"
                True
                (C
                    [ createNode "bin"
                        (C
                            [ createNode "date" (C [])
                            , createNode "echo" (C [])
                            , createNode "cd" (C [])
                            , createNode "diff" (C [])
                            ]
                        )
                    , createNode "usr"
                        (C
                            [ createNode "bin" (C [])
                            , createNode "etc" (C [])
                            , createNode "games" (C [])
                            , createNode "lib" (C [])
                            ]
                        )
                    , createNode "boot" (C [])
                    , createNode "home"
                        (C
                            [ createNode "zgeorg03"
                                (C
                                    [ createNode "Desktop" (C [])
                                    , createNode "Downloads" (C [])
                                    , createNode "Music" (C [])
                                    ]
                                )
                            , createNode "test123" (C [])
                            , createNode "anotherTest" (C [])
                            ]
                        )
                    ]
                )
                []
    in
        constructPath ( [], x )


createNode : String -> Children -> Node
createNode title children =
    Node title False children []


init : String -> Children -> Model
init name children =
    let
        x =
            createLinux
    in
        Model x x


initCmd : String -> Children -> ( Model, Cmd Msg )
initCmd name children =
    ( init name children
    , Cmd.none
    )


main : Program Never
main =
    App.program
        { init = initCmd "/" (C [])
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }
